{-# LANGUAGE OverloadedStrings #-}

module Lib (main) where

import Render
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
    world'' <- renderWorld renderer world'
    when continue $ go world''

initialWorld :: World
initialWorld = World
  { playerChunk = 0
  , playerPos = 0
  , chunkGlobals = mempty
  , chunkLocals = mempty
  , mapView = Local
  }
