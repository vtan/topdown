module WorldGen where

import Spaces
import World

import Control.Monad.Random
import Data.Hashable (hash)

import qualified Data.Set as Set



generateChunk :: ChnIdx Int -> (ChunkGlobal, ChunkLocal)
generateChunk pos = evalRand randomChunk (mkStdGen seed)
  where
    seed = hash pos

randomChunk :: MonadRandom m => m (ChunkGlobal, ChunkLocal)
randomChunk = do
  density <- getRandom
  let probability = 0.2 * density
  let treeOnTile _tile = (< probability) <$> getRandom
  trees <- filterM treeOnTile chunkRelPositions
  pure
    ( ChunkGlobal { chunkGlobalTreeDensity = density }
    , ChunkLocal { chunkLocalTreeRelPositions = Set.fromList trees }
    )
