module Lib.Update where

import Lib.ChunkData
import Lib.ChunkGen
import Lib.Spaces
import Lib.World

import Control.Arrow ((&&&))
import Control.Lens
import Control.Monad.Random (MonadRandom, getRandomR)
import Data.Foldable (foldlM)
import Data.Hashable (hash)
import Data.Ix (range)
import Data.List (foldl', sort)
import SDL

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
  Local -> shootArrow posChn world
  where
    posChn = view (from _TileV) $ scrToTiles tileSize eyeScr (world ^. playerPos . _TileV) posScr
    eyeScr = (`quot` 2) <$> screenSize - tileSize

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
shootArrow target world
  | dist >= 1.0 && dist <= maxDist = do
    arrow <- (source +) <$> deviatedArrow dist dir
    let (arrowIdx, arrowChn) = normalizeChunkPos (world ^. playerChunk) arrow
    pure $ world
      & loadedChunkLocals . at arrowIdx . _Just
      . objects . at arrowChn %~ \case
        Just objs -> Just $ objs ++ [Arrow]
        Nothing -> Just [Arrow]
  | otherwise = pure world
  where
    dist = sqrt $ quadrance playerToTarget
    dir = (let InChunkV (V2 x y) = playerToTarget in atan2 y x)
    playerToTarget :: InChunkV Float
    playerToTarget = fromIntegral <$> target - source
    source = world ^. playerPos
    maxDist = 20.0

deviatedArrow :: MonadRandom m => Float -> Float -> m (InChunkV Int)
deviatedArrow targetDist targetDir = do
  distDeviation <- getRandomR (-maxDistMultiplier, maxDistMultiplier)
  dirDeviation <- getRandomR (-maxDirDeviation, maxDirDeviation)
  let dist = targetDist + distDeviation * targetDist
      dir = targetDir + dirDeviation
  pure . InChunkV $ round <$> dist *^ angle dir
  where
    maxDistMultiplier = 0.3
    maxDirDeviation = targetDist / 12.0 * pi / 4.0

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



chunkLoadRadius :: Num a => a
chunkLoadRadius = 3

maxLoadedChunks :: Int
maxLoadedChunks = 2 * (2 * chunkLoadRadius + 1) ^ (2 :: Int)
