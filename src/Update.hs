{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Update where

import Chunk
import Spaces
import World

import Control.Lens
import Control.Monad.Random
import Control.Monad.State
import Data.Foldable (foldlM)
import SDL

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
movePlayerGlobal dir world = world & playerChunk +~ dir

movePlayerLocal :: Chn2 Int -> World -> World
movePlayerLocal dir world = flip execState world $ do
  (_, chunkLocal) <- getChunkAt i'
  unless (chunkLocal ^. trees . contains pos') $ do
    playerChunk .= i'
    playerPos .= pos'
  where
    (i', pos') = normalizeChunkPos
      (world ^. playerChunk)
      (world ^. playerPos + dir)

toggleMapView :: World -> World
toggleMapView world = case world ^. mapView of
  Global ->
    let
      emptyPossInChunk =
        filter (`Set.notMember` (chunkLocal ^. trees)) chunkRelPositions
      ((_, chunkLocal), world') = runState getPlayerChunk world
    in case emptyPossInChunk of
      pos:_ -> world'
        & mapView .~ Local
        & playerPos .~ pos
      [] -> world'
  Local -> world & mapView .~ Global

shootArrow :: MonadRandom m => Chn2 Int -> World -> m World
shootArrow target world
  | dist >= 1.0 && dist <= maxDist
    = flip execStateT world $ do
      arrow <- (source +) <$> deviatedArrow dist dir
      let (arrowIdx, arrowChn) = normalizeChunkPos (world ^. playerChunk) arrow
      void $ getChunkAt arrowIdx
      chunkLocals . at arrowIdx . _Just . arrows . contains arrowChn .= True
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
