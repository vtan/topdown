{-# LANGUAGE OverloadedStrings #-}

module Lib (main) where

import Render
import Update
import World

import Control.Monad
import Control.Monad.Random
import SDL



main :: IO ()
main = do
  initializeAll
  window <- createWindow "topdown" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  flip fix initialWorld $ \go world -> do
    events <- pollEvents
    let continue = null [() | Event { eventPayload = QuitEvent } <- events]
    world' <- evalRandIO $ updateWorld events world
    world'' <- renderWorld renderer world'
    when continue $ go world''

initialWorld :: World
initialWorld = World
  { worldPlayerChunk = 0
  , worldPlayerPos = 0
  , worldChunkGlobals = mempty
  , worldChunkLocals = mempty
  , worldMapView = Local
  }
