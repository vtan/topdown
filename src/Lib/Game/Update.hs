module Lib.Game.Update where

import Lib.Game.ChunkGen
import Lib.Model.Lenses
import Lib.Model.Spaces
import Lib.Model.Types
import Lib.Model.World
import Lib.Util

import Control.Arrow ((&&&))
import Control.Lens
import Control.Monad.Random (MonadRandom)
import Data.Foldable (foldl', foldlM)
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
      [ ScreenV $ fromIntegral <$> pos
      | Event _ (MouseButtonEvent (MouseButtonEventData
        { mouseButtonEventMotion = Pressed
        , mouseButtonEventButton = ButtonLeft
        , mouseButtonEventPos = P pos
        })) <- events
      ]



applyKeyPress :: MonadRandom m => World -> Scancode -> m World
applyKeyPress world scancode = pure $
  case scancode of
    _ | has (_activeDropdown . _Just) world -> world
    (scancodeToDir -> Just dir) ->
      case world ^. _mapView of
        Global -> movePlayerGlobal (ChunkV dir) world
        Local -> movePlayerLocal (InChunkV dir) world
    ScancodeTab -> toggleMapView world
    _ -> world

applyMouseClick :: MonadRandom m => World -> ScreenV Int -> m World
applyMouseClick world posScr = case world ^. _mapView of
  Global -> pure world
  Local -> case world ^. _activeDropdown of
    Just d -> applyDropdownClick posScr d world
    Nothing ->
      case items of
        [] -> pure world
        _ -> pure $ set _activeDropdown dropdown world
  where
    neighbor = withinRadius 1 pos (world ^. _playerPos)
    pos = floor <$> fracPos
    fracPos = view (from _TileV) $ scrToTiles tileSize eyeScr (world ^. _playerPos . _TileV) posScr
    (chunk, posNorm) = normalizeChunkPos (world ^. _playerChunk) pos
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
      | (validChunk chunk && world ^. _playerPos /= pos) =
          [ShootArrow pos | has (_inventory . at Arrow . _Just) world]
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
applyDropdownClick clickPos (Dropdown anchor cmds) world =
  applyCont . set _activeDropdown Nothing $ world
  where
    applyCont = case clickedItem of
      Just (GetObject chunk pos obj) -> pure . getObject chunk pos obj
      Just (ShootArrow relPos) -> shootArrow relPos
      Just (TradeObjects gq go rq ro) -> pure . tradeObjects gq go rq ro
      Nothing -> pure
    clickedItem = fmap snd . flip ifind cmds $ \i _ ->
      let topLeft = anchorScr + screenV 0 (i * dropdownItemSize ^. _y)
      in inRectangle clickPos (topLeft, topLeft + dropdownItemSize)
    anchorScr = floor <$> localTileToScreen world anchor



movePlayerGlobal :: ChunkV Int -> World -> World
movePlayerGlobal dir world
  | validChunk i' = world
    & _playerChunk .~ i'
    & loadChunksNearPlayer
  | otherwise = world
  where
    i' = (world ^. _playerChunk) + dir

movePlayerLocal :: InChunkV Int -> World -> World
movePlayerLocal dir world = case world ^. _loadedChunkLocals . at i' of
  Just chunkLocal | passableTile chunkLocal pos' -> world
    & set _playerChunk i'
    & set _playerPos pos'
    & loadChunksNearPlayer
  _ -> world
  where
    (i', pos') = normalizeChunkPos
      (world ^. _playerChunk)
      (world ^. _playerPos + dir)

toggleMapView :: World -> World
toggleMapView world = case world ^. _mapView of
  Global ->
    let i = world ^. _playerChunk
    in case world ^. _loadedChunkLocals . at i of
      Just chunkLocal ->
        let passableTiles = filter (passableTile chunkLocal) chunkRelPositions
        in case passableTiles of
          pos:_ -> world
            & _mapView .~ Local
            & _playerPos .~ pos
          [] -> world
      Nothing ->
        world
  Local -> world & _mapView .~ Global

loadChunksNearPlayer :: World -> World
loadChunksNearPlayer world = world
  & _loadedChunkLocals %~ (\x -> foldl' load x nearLocals)
  & unloadFarChunks
  where
    load locals idx = locals & at idx %~ \case
      x@Just{} -> x
      Nothing ->
        let global = globals ^. singular (ix idx)
            seed = hash idx
        in Just $ generateChunkLocal seed global
    globals = world ^. _chunkGlobals
    nearLocals = filter validChunk
      $ (+) (world ^. _playerChunk)
      <$> range ((-chunkLoadRadius), chunkLoadRadius)

unloadFarChunks :: World -> World
unloadFarChunks world 
  | noChunksToUnload > 0 = world
    & _loadedChunkLocals %~ (\x -> foldl' (flip Map.delete) x farChunks)
  | otherwise = world
  where
    locals = world ^. _loadedChunkLocals
    noChunksToUnload = Map.size locals - maxLoadedChunks
    farChunks = map snd . take noChunksToUnload
      . reverse . List.sort
      . map (qd (world ^. _playerChunk) &&& id)
      . Set.toList . Map.keysSet $ locals

shootArrow :: MonadRandom m => InChunkV Int -> World -> m World
shootArrow target world = do
  hit <- randomChance hitChance
  hitPos <- if hit
    then pure target
    else Random.uniform neighbors
  let (hitChunk, hitPosNorm) = normalizeChunkPos (world ^. _playerChunk) hitPos
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
  $ cl ^. _objects . at p . _Just

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
