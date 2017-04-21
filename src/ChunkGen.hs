module ChunkGen where

import ChunkData
import Spaces

import Control.Lens.Operators
import Control.Monad (filterM)
import Control.Monad.Random (evalRand, getRandom, mkStdGen)

import qualified Data.Set as Set



generateChunkLocal :: Int -> ChunkGlobal -> ChunkLocal
generateChunkLocal seed global = flip evalRand (mkStdGen seed) $ do
  let probability = 0.2 * (global ^. treeDensity)
      treeOnTile _tile = (< probability) <$> getRandom
  randomTrees <- filterM treeOnTile chunkRelPositions
  pure ChunkLocal
    { chunkLocalTrees = Set.fromList randomTrees
    , chunkLocalArrows = mempty
    }
