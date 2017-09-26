module Lib.Main (main) where

import Lib.Render
import Lib.Update
import Lib.WorldGen

import Control.Monad (when)
import Control.Monad.Random (evalRandIO)
import Data.Function (fix)
import SDL

import qualified SDL.Font as Sdl.Font



main :: IO ()
main = do
  initializeAll
  Sdl.Font.initialize
  window <- createWindow "topdown" defaultWindow
  let rendererConfig = defaultRenderer
        { rendererType = AcceleratedVSyncRenderer }
  renderer <- createRenderer window (-1) rendererConfig
  font <- Sdl.Font.load fontPath 12
  world0 <- loadChunksNearPlayer <$> evalRandIO initialWorld
  flip fix world0 $ \go world -> do
    events <- pollEvents
    let continue = null [() | Event { eventPayload = QuitEvent } <- events]
    world' <- evalRandIO $ updateWorld events world
    renderWorld renderer font world'
    when continue $ go world'

fontPath :: FilePath
fontPath = "data/liberation-fonts-ttf-2.00.1/LiberationSans-Regular.ttf"
