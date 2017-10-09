module Lib.WorldGen (initialWorld) where

import Lib.Model.Spaces
import Lib.Model.Types
import Lib.World

import Control.Monad (forM)
import Control.Monad.Random

import qualified Data.Array as Array
import qualified Data.Ix as Ix



initialWorld :: MonadRandom m => m World
initialWorld = do
  assocs <- forM (Ix.range (0, worldSize - 1)) $ \i ->
    (i,) <$> generateChunkGlobal i
  pure World
    { worldPlayerChunk = 0
    , worldPlayerPos = 0
    , worldChunkGlobals = Array.array (0, worldSize - 1) assocs
    , worldLoadedChunkLocals = mempty
    , worldMapView = Local
    , worldInventory = mempty
    , worldActiveDropdown = Nothing
    }



generateChunkGlobal :: MonadRandom m => ChunkV Int -> m ChunkGlobal
generateChunkGlobal chunk =
  ChunkGlobal <$> getRandom <*> hasVillage
  where
    hasVillage = all ((== 0) . (`mod` 10)) <$> perturbedChunk
    perturbedChunk = (chunk +) <$> (chunkV <$> d <*> d)
    d = getRandomR ((-3), 3)
