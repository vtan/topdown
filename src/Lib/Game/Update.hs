module Lib.Game.Update where

import Lib.Game.ChunkGen
import Lib.Model.Spaces
import Lib.Model.Types
import Lib.Model.World
import Lib.Util

import Control.Arrow ((&&&))
import Control.Lens
import Control.Monad.Random (MonadRandom)
import Data.Foldable (foldl', foldlM)
import Data.Generics.Product (field)
import Data.Hashable (hash)
import Data.Ix (range)
import SDL

import qualified Control.Monad.Random as Random
import qualified Data.List as List
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
      [ fromIntegral <$> pos
      | Event _ (MouseButtonEvent (MouseButtonEventData
        { mouseButtonEventMotion = Pressed
        , mouseButtonEventButton = ButtonLeft
        , mouseButtonEventPos = P pos
        })) <- events
      ]



applyKeyPress :: MonadRandom m => World -> Scancode -> m World
applyKeyPress world scancode = pure $
  case scancode of
    _ | has (field @"activeDropdown" . _Just) world -> world
    (scancodeToDir -> Just dir) ->
      case world ^. field @"mapView" of
        Global -> movePlayerGlobal (Chunk <$> dir) world
        Local -> movePlayerLocal (InChunk <$> dir) world
    ScancodeTab -> toggleMapView world
    _ -> world

applyMouseClick :: MonadRandom m => World -> ScreenV Int -> m World
applyMouseClick world posScr = case world ^. field @"mapView" of
  Global -> pure world
  Local -> case world ^. field @"activeDropdown" of
    Just d -> applyDropdownClick posScr d world
    Nothing ->
      case items of
        [] -> pure world
        _ -> pure $ set (field @"activeDropdown") dropdown world
  where
    neighbor = withinRadius 1 pos (world ^. field @"playerPos")
    pos = floor <$> fracPos
    fracPos = InChunk . unTile <$> scrToTiles tileSize eyeScr (fromIntegral <$> world ^. field @"playerPos") posScr
    (chunk, posNorm) = normalizeChunkPos (world ^. field @"playerChunk") pos
    eyeScr = (`quot` 2) <$> screenSize - tileSize
    dropdown = case items of
      [] -> Nothing
      _ -> Just $ Dropdown fracPos items
    items = tradeItems ++ getObjItems ++ shootArrowItems
    getObjItems
      | neighbor =
          map (\o -> GetObject chunk posNorm o)
          . List.sort
          . filter storable
          $ world ^. objectsAt chunk posNorm
      | otherwise = []
    shootArrowItems
      | (validChunk chunk && world ^. field @"playerPos" /= pos) =
          [ShootArrow pos | has (field @"inventory" . at Arrow . _Just) world]
      | otherwise = []
    tradeItems
      | neighbor && elemOf (objectsAt chunk posNorm . folded) Villager world =
          [ TradeObjects 3 Meat 2 Gold
            | world ^. objectsInInventory Meat >= 3
          ] ++ [ TradeObjects 2 Gold 1 Meat
            | world ^. objectsInInventory Gold >= 2
          ]
      | otherwise = []

applyDropdownClick :: MonadRandom m => ScreenV Int -> Dropdown -> World -> m World
applyDropdownClick clickPos Dropdown{ anchor, commands } world =
  applyCont . set (field @"activeDropdown") Nothing $ world
  where
    applyCont = case clickedItem of
      Just (GetObject chunk pos obj) -> pure . getObject chunk pos obj
      Just (ShootArrow relPos) -> shootArrow relPos
      Just (TradeObjects gq go rq ro) -> pure . tradeObjects gq go rq ro
      Nothing -> pure
    clickedItem = fmap snd . flip ifind commands $ \i _ ->
      let topLeft = anchorScr + V2 0 (i *^ dropdownItemSize ^. _y)
      in inRectangle clickPos (topLeft, topLeft + dropdownItemSize)
    anchorScr = floor <$> localTileToScreen world anchor



movePlayerGlobal :: ChunkV Int -> World -> World
movePlayerGlobal dir world
  | validChunk i' = world
    & field @"playerChunk" .~ i'
    & loadChunksNearPlayer
  | otherwise = world
  where
    i' = (world ^. field @"playerChunk") + dir

movePlayerLocal :: InChunkV Int -> World -> World
movePlayerLocal dir world = case world ^. field @"loadedChunkLocals" . at i' of
  Just chunkLocal | passableTile chunkLocal pos' -> world
    & set (field @"playerChunk") i'
    & set (field @"playerPos") pos'
    & loadChunksNearPlayer
  _ -> world
  where
    (i', pos') = normalizeChunkPos
      (world ^. field @"playerChunk")
      (world ^. field @"playerPos" + dir)

toggleMapView :: World -> World
toggleMapView world = case world ^. field @"mapView" of
  Global ->
    let i = world ^. field @"playerChunk"
    in case world ^. field @"loadedChunkLocals" . at i of
      Just chunkLocal ->
        let passableTiles = filter (passableTile chunkLocal) chunkRelPositions
        in case passableTiles of
          pos:_ -> world
            & field @"mapView" .~ Local
            & field @"playerPos" .~ pos
          [] -> world
      Nothing ->
        world
  Local -> world & field @"mapView" .~ Global

loadChunksNearPlayer :: World -> World
loadChunksNearPlayer world = world
  & field @"loadedChunkLocals" %~ (\x -> foldl' load x nearLocals)
  & unloadFarChunks
  where
    load locals idx = locals & at idx %~ \case
      x@Just{} -> x
      Nothing ->
        let global = globals ^. singular (ix idx)
            seed = hash idx
        in Just $ generateChunkLocal seed global
    globals = world ^. field @"chunkGlobals"
    nearLocals = filter validChunk
      $ (+) (world ^. field @"playerChunk")
      <$> range ((-chunkLoadRadius), chunkLoadRadius)

unloadFarChunks :: World -> World
unloadFarChunks world 
  | noChunksToUnload > 0 = world
    & field @"loadedChunkLocals" %~ (\x -> foldl' (flip Map.delete) x farChunks)
  | otherwise = world
  where
    locals = world ^. field @"loadedChunkLocals"
    noChunksToUnload = Map.size locals - maxLoadedChunks
    farChunks = map snd . take noChunksToUnload
      . reverse . List.sort
      . map (qd (world ^. field @"playerChunk") &&& id)
      . Set.toList . Map.keysSet $ locals

shootArrow :: MonadRandom m => InChunkV Int -> World -> m World
shootArrow target world = do
  hit <- randomChance hitChance
  hitPos <- if hit
    then pure target
    else Random.uniform neighbors
  let (hitChunk, hitPosNorm) = normalizeChunkPos (world ^. field @"playerChunk") hitPos
  pure
    . over (objectsAt hitChunk hitPosNorm) shootArrowAt
    . over (objectsInInventory Arrow) (subtract 1)
    $ world
  where
    hitChance = 0.7
    neighbors = [target + v | v <- range ((-1), 1), v /= 0]

shootArrowAt :: [Object] -> [Object]
shootArrowAt objs
  | elem Deer objs = filter (/= Deer) objs |> Meat
  | otherwise = objs |> Arrow

getObject :: ChunkV Int -> InChunkV Int -> Object -> World -> World
getObject chunk pos obj =
  (objectsInInventory obj +~ 1)
  . (objectsAt chunk pos %~ List.delete obj)

tradeObjects :: Int -> Object -> Int -> Object -> World -> World
tradeObjects givenQty givenObj recvdQty recvdObj =
  (objectsInInventory givenObj -~ givenQty)
  . (objectsInInventory recvdObj +~ recvdQty)

scancodeToDir :: Num a => Scancode -> Maybe (V2 a)
scancodeToDir = \case
  ScancodeRight -> Just $ unit _x
  ScancodeLeft -> Just . negate $ unit _x
  ScancodeUp -> Just $ unit _y
  ScancodeDown -> Just . negate $ unit _y
  _ -> Nothing

passableTile :: ChunkLocal -> InChunkV Int -> Bool
passableTile cl p = null . filter (not . passable)
  $ cl ^. field @"objects" . at p . _Just

passable :: Object -> Bool
passable = \case
  Tree -> False
  Arrow -> True
  Deer -> False
  Meat -> True
  Wall -> False
  Villager -> False
  Gold -> True

storable :: Object -> Bool
storable = \case
  Arrow -> True
  Meat -> True
  Gold -> True
  Deer -> False
  Tree -> False
  Wall -> False
  Villager -> False



chunkLoadRadius :: Num a => a
chunkLoadRadius = 3

maxLoadedChunks :: Int
maxLoadedChunks = 2 * (2 * chunkLoadRadius + 1) ^ (2 :: Int)
