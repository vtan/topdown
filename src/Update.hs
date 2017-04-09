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
applyKeyPress world scancode = move world
  where
    move = case scancode of
      ScancodeRight -> movePlayer $ unit _x
      ScancodeLeft -> movePlayer . negate $ unit _x
      ScancodeDown -> movePlayer $ unit _y
      ScancodeUp -> movePlayer . negate $ unit _y
      _ -> id

movePlayer :: Chn2 Int -> World -> World
movePlayer dir world = flip execState world $ do
  chunk' <- state $ getChunkAt i'
  unless (Set.member pos' $ treeRelPositions chunk') $
    modify' $ \w -> w { playerChunk = i', playerPos = pos' }
  where
    (i', pos') = normalizeChunkPos (playerChunk world) (playerPos world + dir)
