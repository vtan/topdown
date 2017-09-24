module Lib.ChunkGen where

import Lib.ChunkData
import Lib.Spaces
import Lib.Util

import Control.Lens.Operators
import Control.Monad (filterM)
import Control.Monad.Random (evalRand, mkStdGen)
import Data.Foldable
import Data.Monoid

import qualified Control.Monad.Random as Random
import qualified Data.Map as Map



generateChunkLocal :: Int -> ChunkGlobal -> ChunkLocal
generateChunkLocal seed global = flip evalRand (mkStdGen seed) $ do
  deerPos <- randomChance deerChance >>= \case
    True -> Just <$> Random.uniform chunkRelPositions
    False -> pure Nothing
  treePos <- filterM (const $ randomChance treeChance) chunkRelPositions
  let deers = map (, [Deer]) $ toList deerPos
      trees = map (, [Tree]) treePos
  pure ChunkLocal
    { chunkLocalObjects =
        Map.fromList deers
        <> Map.fromList trees
    }
  where
    -- Per block in chunk
    treeChance = 0.2 * (global ^. treeDensity)
    -- Per chunk
    deerChance = 0.1
