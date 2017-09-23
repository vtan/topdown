module Lib.Main (main) where

import Lib.Render
import Lib.Update
import Lib.WorldGen

import Control.Monad (when)
import Control.Monad.Random (evalRandIO)
import Data.Function (fix)
import Data.String (fromString)
import SDL



main :: IO ()
main = do
  initializeAll
  window <- createWindow (fromString "topdown") defaultWindow
  let rendererConfig = defaultRenderer
        { rendererType = AcceleratedVSyncRenderer }
  renderer <- createRenderer window (-1) rendererConfig
  world0 <- loadChunksNearPlayer <$> evalRandIO initialWorld
  flip fix world0 $ \go world -> do
    events <- pollEvents
    let continue = null [() | Event { eventPayload = QuitEvent } <- events]
    world' <- evalRandIO $ updateWorld events world
    renderWorld renderer world'
    when continue $ go world'
