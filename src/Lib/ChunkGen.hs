{-# LANGUAGE TupleSections #-}

module Lib.ChunkGen where

import Lib.ChunkData
import Lib.Spaces

import Control.Lens.Operators
import Control.Monad (filterM)
import Control.Monad.Random (evalRand, getRandom, mkStdGen)

import qualified Data.Map as Map



generateChunkLocal :: Int -> ChunkGlobal -> ChunkLocal
generateChunkLocal seed global = flip evalRand (mkStdGen seed) $ do
  let probability = 0.2 * (global ^. treeDensity)
      treeOnTile _tile = (< probability) <$> getRandom
  randomTrees <- filterM treeOnTile chunkRelPositions
  pure ChunkLocal
    { chunkLocalObjects = Map.fromList . map (, [Tree]) $ randomTrees
    }
