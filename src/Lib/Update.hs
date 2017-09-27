module Lib.Update where

import Lib.ChunkData
import Lib.ChunkGen
import Lib.Spaces
import Lib.Util
import Lib.World

import Control.Arrow ((&&&))
import Control.Lens
import Control.Monad.Random (MonadRandom)
import Data.Foldable (foldlM)
import Data.Hashable (hash)
import Data.Ix (range)
import Data.List (foldl', sort)
import SDL

import qualified Control.Monad.Random as Random
import qualified Data.Map as Map
import qualified Data.Set as Set



updateWorld :: MonadRandom m => [Event] -> World -> m World
updateWorld events world = do
  world' <- foldlM applyKeyPress world keyPresses
  foldlM applyMouseClick world' mouseClicks
  where
    keyPresses =
      [ scancode
      | Event _ (KeyboardEvent (KeyboardEventData
        { keyboardEventKeyMotion = Pressed
        , keyboardEventKeysym = Keysym { keysymScancode = scancode }
        })) <- events
      ]
    mouseClicks =
      [ ScreenV $ fromIntegral <$> pos
      | Event _ (MouseButtonEvent (MouseButtonEventData
        { mouseButtonEventMotion = Pressed
        , mouseButtonEventButton = ButtonLeft
        , mouseButtonEventPos = P pos
        })) <- events
      ]



applyKeyPress :: MonadRandom m => World -> Scancode -> m World
applyKeyPress world = pure . \case
  (scancodeToDir -> Just dir) -> case world ^. mapView of
    Global -> movePlayerGlobal (ChunkV dir) world
    Local -> movePlayerLocal (InChunkV dir) world
  ScancodeTab -> toggleMapView world
  _ -> world

applyMouseClick :: MonadRandom m => World -> ScreenV Int -> m World
applyMouseClick world posScr = case world ^. mapView of
  Global -> pure world
  Local -> case world ^. activeDropdown of
    Just d -> applyDropdownClick posScr d world
    Nothing ->
      case items of
        [] -> pure world
        _ -> pure $ set activeDropdown dropdown world
  where
    withinGetDistance = withinRadius 1 pos (world ^. playerPos)
    pos = floor <$> fracPos
    fracPos = view (from _TileV) $ scrToTiles tileSize eyeScr (world ^. playerPos . _TileV) posScr
    (chunk, posNorm) = normalizeChunkPos (world ^. playerChunk) pos
    eyeScr = (`quot` 2) <$> screenSize - tileSize
    dropdown = case items of
      [] -> Nothing
      _ -> Just $ Dropdown fracPos items
    items = getObjItems ++ shootArrowItems
    getObjItems
      | withinGetDistance =
          map (\o -> DropdownItem (GetObject o) (pure . getObject o chunk posNorm))
          . sort
          . filter storable
          $ world ^. objectsAt chunk posNorm
      | otherwise = []
    shootArrowItems
      | (validChunk chunk && world ^. playerPos /= pos) =
          [DropdownItem ShootArrow $ shootArrow pos]
      | otherwise = []

applyDropdownClick :: MonadRandom m => ScreenV Int -> Dropdown -> World -> m World
applyDropdownClick clickPos (Dropdown anchor cmds) world =
  applyCont . set activeDropdown Nothing $ world
  where
    applyCont = case clickedItem of
      Just (DropdownItem _ cont) -> cont
      Nothing -> pure
    clickedItem = fmap snd . flip ifind cmds $ \i _ ->
      let topLeft = anchorScr + screenV 0 (i * dropdownItemSize ^. _y)
      in inRectangle clickPos (topLeft, topLeft + dropdownItemSize)
    anchorScr = floor <$> localTileToScreen world anchor



movePlayerGlobal :: ChunkV Int -> World -> World
movePlayerGlobal dir world
  | validChunk i' = world
    & playerChunk .~ i'
    & loadChunksNearPlayer
  | otherwise = world
  where
    i' = (world ^. playerChunk) + dir

movePlayerLocal :: InChunkV Int -> World -> World
movePlayerLocal dir world = case world ^. loadedChunkLocals . at i' of
  Just chunkLocal | passableTile chunkLocal pos' -> world
    & set playerChunk i'
    & set playerPos pos'
    & loadChunksNearPlayer
  _ -> world
  where
    (i', pos') = normalizeChunkPos
      (world ^. playerChunk)
      (world ^. playerPos + dir)

toggleMapView :: World -> World
toggleMapView world = case world ^. mapView of
  Global ->
    let i = world ^. playerChunk
    in case world ^. loadedChunkLocals . at i of
      Just chunkLocal ->
        let passableTiles = filter (passableTile chunkLocal) chunkRelPositions
        in case passableTiles of
          pos:_ -> world
            & mapView .~ Local
            & playerPos .~ pos
          [] -> world
      Nothing ->
        world
  Local -> world & mapView .~ Global

loadChunksNearPlayer :: World -> World
loadChunksNearPlayer world = world
  & loadedChunkLocals %~ (\x -> foldl' load x nearLocals)
  & unloadFarChunks
  where
    load locals idx = locals & at idx %~ \case
      x@Just{} -> x
      Nothing ->
        let global = globals ^. arrayAt idx
            seed = hash idx
        in Just $ generateChunkLocal seed global
    globals = world ^. chunkGlobals
    nearLocals = filter validChunk
      $ (+) (world ^. playerChunk)
      <$> range ((-chunkLoadRadius), chunkLoadRadius)

unloadFarChunks :: World -> World
unloadFarChunks world 
  | noChunksToUnload > 0 = world
    & loadedChunkLocals %~ (\x -> foldl' (flip Map.delete) x farChunks)
  | otherwise = world
  where
    locals = world ^. loadedChunkLocals
    noChunksToUnload = Map.size locals - maxLoadedChunks
    farChunks = map snd . take noChunksToUnload
      . reverse . sort
      . map (qd (world ^. playerChunk) &&& id)
      . Set.toList . Map.keysSet $ locals

shootArrow :: MonadRandom m => InChunkV Int -> World -> m World
shootArrow target world = do
  hit <- randomChance hitChance
  hitPos <- if hit
    then pure target
    else Random.uniform neighbors
  let (hitChunk, hitPosNorm) = normalizeChunkPos (world ^. playerChunk) hitPos
  pure $ over (objectsAt hitChunk hitPosNorm) shootArrowAt world
  where
    hitChance = 0.7
    neighbors = [target + v | v <- range ((-1), 1), v /= 0]

shootArrowAt :: [Object] -> [Object]
shootArrowAt objs
  | elem Deer objs = filter (/= Deer) objs |> Meat
  | otherwise = objs |> Arrow

getObject :: Object -> ChunkV Int -> InChunkV Int -> World -> World
getObject obj chunk pos =
  over (inventory . at obj . non 0) (+1)
  . over (objectsAt chunk pos) (filter (/= obj))

scancodeToDir :: Num a => Scancode -> Maybe (V2 a)
scancodeToDir = \case
  ScancodeRight -> Just $ unit _x
  ScancodeLeft -> Just . negate $ unit _x
  ScancodeUp -> Just $ unit _y
  ScancodeDown -> Just . negate $ unit _y
  _ -> Nothing

passableTile :: ChunkLocal -> InChunkV Int -> Bool
passableTile cl p = null . filter (not . passable)
  $ cl ^. objects . at p . _Just

passable :: Object -> Bool
passable = \case
  Tree -> False
  Arrow -> True
  Deer -> False
  Meat -> True

storable :: Object -> Bool
storable = \case
  Arrow -> True
  Meat -> True
  Deer -> False
  Tree -> False



chunkLoadRadius :: Num a => a
chunkLoadRadius = 3

maxLoadedChunks :: Int
maxLoadedChunks = 2 * (2 * chunkLoadRadius + 1) ^ (2 :: Int)
