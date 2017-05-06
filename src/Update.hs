{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Update where

import ChunkData
import ChunkGen
import Spaces
import World

import Control.Arrow ((&&&))
import Control.Lens (_Just, at, contains, set)
import Control.Lens.Operators
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
      [ Scr2 $ fromIntegral <$> pos
      | Event _ (MouseButtonEvent (MouseButtonEventData
        { mouseButtonEventMotion = Pressed
        , mouseButtonEventButton = ButtonLeft
        , mouseButtonEventPos = P pos
        })) <- events
      ]



applyKeyPress :: MonadRandom m => World -> Scancode -> m World
applyKeyPress world = pure . \case
  (scancodeToDir -> Just dir) -> case world ^. mapView of
    Global -> movePlayerGlobal (ChnIdx dir) world
    Local -> movePlayerLocal (Chn2 dir) world
  ScancodeTab -> toggleMapView world
  _ -> world

applyMouseClick :: MonadRandom m => World -> Scr2 Int -> m World
applyMouseClick world posScr = case world ^. mapView of
  Global -> pure world
  Local -> shootArrow posChn world
  where
    posChn = scrToTiles tileSize eyeScr (world ^. playerPos) posScr
    eyeScr = (`quot` 2) <$> screenSize - tileSize

movePlayerGlobal :: ChnIdx Int -> World -> World
movePlayerGlobal dir world
  | validChunk i' = world
    & playerChunk .~ i'
    & loadChunksNearPlayer
  | otherwise = world
  where
    i' = (world ^. playerChunk) + dir

movePlayerLocal :: Chn2 Int -> World -> World
movePlayerLocal dir world = case world ^. loadedChunkLocals . at i' of
  Just chunkLocal | not (chunkLocal ^. trees . contains pos') -> world
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
        let noTrees = (`Set.notMember` (chunkLocal ^. trees))
            emptyPossInChunk = filter noTrees chunkRelPositions
        in case emptyPossInChunk of
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

shootArrow :: MonadRandom m => Chn2 Int -> World -> m World
shootArrow target world
  | dist >= 1.0 && dist <= maxDist = do
    arrow <- (source +) <$> deviatedArrow dist dir
    let (arrowIdx, arrowChn) = normalizeChunkPos (world ^. playerChunk) arrow
    pure $ world
      & loadedChunkLocals . at arrowIdx . _Just
      . arrows . contains arrowChn .~ True
  | otherwise = pure world
  where
    dist = sqrt $ quadrance playerToTarget
    dir = (let Chn2 (V2 x y) = playerToTarget in atan2 y x)
    playerToTarget :: Chn2 Float
    playerToTarget = fromIntegral <$> target - source
    source = world ^. playerPos
    maxDist = 20.0

deviatedArrow :: MonadRandom m => Float -> Float -> m (Chn2 Int)
deviatedArrow targetDist targetDir = do
  distDeviation <- getRandomR (-maxDistMultiplier, maxDistMultiplier)
  dirDeviation <- getRandomR (-maxDirDeviation, maxDirDeviation)
  let dist = targetDist + distDeviation * targetDist
      dir = targetDir + dirDeviation
  pure . Chn2 $ round <$> dist *^ angle dir
  where
    maxDistMultiplier = 0.3
    maxDirDeviation = targetDist / 12.0 * pi / 4.0

scancodeToDir :: Num a => Scancode -> Maybe (V2 a)
scancodeToDir = \case
  ScancodeRight -> Just $ unit _x
  ScancodeLeft -> Just . negate $ unit _x
  ScancodeDown -> Just $ unit _y
  ScancodeUp -> Just . negate $ unit _y
  _ -> Nothing



chunkLoadRadius :: Num a => a
chunkLoadRadius = 3

maxLoadedChunks :: Int
maxLoadedChunks = 2 * (2 * chunkLoadRadius + 1) ^ (2 :: Int)
