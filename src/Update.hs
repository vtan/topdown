{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Update where

import Spaces
import World

import Control.Monad.State
import Data.List (foldl')
import SDL

import qualified Data.Set as Set



updateWorld :: [Event] -> World -> World
updateWorld events world = foldl' applyKeyPress world keyPresses
  where
    keyPresses =
      [ scancode
      | Event _ (KeyboardEvent (KeyboardEventData
        { keyboardEventKeyMotion = Pressed
        , keyboardEventKeysym = Keysym { keysymScancode = scancode }
        })) <- events
      ]



applyKeyPress :: World -> Scancode -> World
applyKeyPress world = \case
  (scancodeToDir -> Just dir) -> case mapView world of
    Global -> movePlayerGlobal (ChnIdx dir) world
    Local -> movePlayerLocal (Chn2 dir) world
  ScancodeTab -> toggleMapView world
  _ -> world

movePlayerGlobal :: ChnIdx Int -> World -> World
movePlayerGlobal dir world = world { playerChunk = playerChunk world + dir }

movePlayerLocal :: Chn2 Int -> World -> World
movePlayerLocal dir world = flip execState world $ do
  (_, chunkLocal) <- getChunkAt i'
  unless (Set.member pos' $ treeRelPositions chunkLocal) $
    modify' $ \w -> w { playerChunk = i', playerPos = pos' }
  where
    (i', pos') = normalizeChunkPos (playerChunk world) (playerPos world + dir)

toggleMapView :: World -> World
toggleMapView world = case mapView world of
  Global ->
    let
      emptyPossInChunk = filter (`Set.notMember` trees) chunkRelPositions
      trees = treeRelPositions chunkLocal
      ((_, chunkLocal), world') = runState getPlayerChunk world
    in case emptyPossInChunk of
      pos:_ -> world' { mapView = Local, playerPos = pos }
      [] -> world'
  Local -> world { mapView = Global }

scancodeToDir :: Num a => Scancode -> Maybe (V2 a)
scancodeToDir = \case
  ScancodeRight -> Just $ unit _x
  ScancodeLeft -> Just . negate $ unit _x
  ScancodeDown -> Just $ unit _y
  ScancodeUp -> Just . negate $ unit _y
  _ -> Nothing
