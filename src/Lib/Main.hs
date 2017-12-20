module Lib.Main (main) where

import Lib.Game.Render
import Lib.Game.Update
import Lib.Game.WorldGen
import Lib.Model.Spaces

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.Random (evalRandIO)
import Data.Function (fix)

import qualified SDL as Sdl
import qualified SDL.TTF as Sdl.Font



main :: IO ()
main = do
  Sdl.initializeAll
  _ <- Sdl.Font.init
  window <- Sdl.createWindow "topdown" windowConfig
  renderer <- Sdl.createRenderer window (-1) rendererConfig
  font <- Sdl.Font.openFont fontPath 12
  world0 <- loadChunksNearPlayer <$> evalRandIO initialWorld
  flip fix world0 $ \go world -> do
    events <- Sdl.pollEvents
    let goLater x = threadDelay 15 *> go x
    case events of
      [] -> goLater world
      _ -> do
        let continue = null
              [() | Sdl.Event { Sdl.eventPayload = Sdl.QuitEvent } <- events]
        world' <- evalRandIO $ updateWorld events world
        renderWorld renderer font world'
        when continue $ goLater world'
  where
  windowConfig = Sdl.defaultWindow
    { Sdl.windowInitialSize = unScreen <$> screenSize }
  rendererConfig = Sdl.defaultRenderer
    { Sdl.rendererType = Sdl.AcceleratedVSyncRenderer }

fontPath :: FilePath
fontPath = "data/liberation-fonts-ttf-2.00.1/LiberationSans-Regular.ttf"
