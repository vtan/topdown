{-# LANGUAGE OverloadedStrings #-}

module Lib (main) where

import Render
import Update
import WorldGen

import Control.Monad (when)
import Control.Monad.Random (evalRandIO)
import Data.Function (fix)
import SDL



main :: IO ()
main = do
  initializeAll
  window <- createWindow "topdown" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  world0 <- evalRandIO initialWorld
  flip fix world0 $ \go world -> do
    events <- pollEvents
    let continue = null [() | Event { eventPayload = QuitEvent } <- events]
    world' <- evalRandIO $ updateWorld events world
    world'' <- renderWorld renderer world'
    when continue $ go world''
