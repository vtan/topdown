{-# LANGUAGE OverloadedStrings #-}

module Lib (main) where

import Render
import Spaces
import Update
import World

import Control.Monad
import Data.Function
import SDL



main :: IO ()
main = do
  initializeAll
  window <- createWindow "topdown" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  flip fix initialWorld $ \go world -> do
    events <- pollEvents
    let
      continue = null [() | Event { eventPayload = QuitEvent } <- events]
      world' = updateWorld events world
    renderWorld renderer world'
    when continue $ go world'

initialWorld :: World
initialWorld = World
  { playerPos = Chn2 $ V2 3 3
  , chunk = generateChunk 0
  }
