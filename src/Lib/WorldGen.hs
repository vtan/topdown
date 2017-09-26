module Lib.WorldGen where

import Lib.ChunkData
import Lib.World

import Control.Monad (forM)
import Control.Monad.Random (MonadRandom, getRandom)

import qualified Data.Array as Array
import qualified Data.Ix as Ix



generateChunkGlobal :: MonadRandom m => m ChunkGlobal
generateChunkGlobal = ChunkGlobal <$> getRandom

initialWorld :: MonadRandom m => m World
initialWorld = do
  assocs <- forM (Ix.range (0, worldSize - 1)) $ \i ->
    (i,) . ChunkGlobal <$> getRandom
  pure World
    { worldPlayerChunk = 0
    , worldPlayerPos = 0
    , worldChunkGlobals = Array.array (0, worldSize - 1) assocs
    , worldLoadedChunkLocals = mempty
    , worldMapView = Local
    , worldInventory = mempty
    }
