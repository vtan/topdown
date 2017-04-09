module Update where

import World

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
applyKeyPress world event = case event of
  ScancodeRight -> movePlayer $ unit _x
  ScancodeLeft -> movePlayer . negate $ unit _x
  ScancodeDown -> movePlayer $ unit _y
  ScancodeUp -> movePlayer . negate $ unit _y
  _ -> world
  where
    movePlayer d
      | Set.member newPos (treeRelPositions $ chunk world) = world
      | otherwise = world { playerPos = playerPos world + d }
      where
        newPos = playerPos world + d
