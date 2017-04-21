{-# LANGUAGE TupleSections #-}

module WorldGen where

import Chunk
import ChunkData
import World

import Control.Monad (forM)
import Control.Monad.Random (MonadRandom, getRandom)

import qualified Data.Array as Array
import qualified Data.Ix as Ix



generateChunkGlobal :: MonadRandom m => m ChunkGlobal
generateChunkGlobal = ChunkGlobal <$> getRandom

initialWorld :: MonadRandom m => m World
initialWorld = do
  assocs <- forM (Ix.range (0, worldSize - 1)) $ \i ->
    (i,) . newChunk . ChunkGlobal <$> getRandom
  pure World
    { worldPlayerChunk = 0
    , worldPlayerPos = 0
    , worldChunks = Array.array (0, worldSize - 1) assocs
    , worldMapView = Local
    }
