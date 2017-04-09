module World where

import Spaces

import Control.Monad.Random
import Data.Hashable (hash)
import Data.Map (Map)
import Data.Set (Set)

import qualified Data.Map as Map
import qualified Data.Set as Set



data World = World
  { playerChunk :: ChnIdx Int
  , playerPos :: Chn2 Int
  , chunks :: Map (ChnIdx Int) MapChunk
  } deriving (Eq, Show)

data MapChunk = MapChunk
  { treeRelPositions :: Set (Chn2 Int)
  } deriving (Eq, Show)

getChunkAt :: ChnIdx Int -> World -> (MapChunk, World)
getChunkAt i w = case Map.lookup i (chunks w) of
  Just chunk -> (chunk, w)
  Nothing ->
    let chunk = generateChunk i
        w' = w { chunks = Map.insert i chunk (chunks w) }
    in (chunk, w')

generateChunk :: ChnIdx Int -> MapChunk
generateChunk pos = evalRand randomChunk (mkStdGen seed)
  where
    seed = hash pos

randomChunk :: MonadRandom m => m MapChunk
randomChunk = do
  treeDensity <- getRandomR (0 :: Float, 0.3)
  let treeOnTile _tile = (< treeDensity) <$> getRandom
  trees <- filterM treeOnTile chunkRelPositions
  pure MapChunk { treeRelPositions = Set.fromList trees }
