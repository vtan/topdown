module World where

import Spaces

import Control.Monad.Random
import Data.Hashable (hash)
import Data.Set (Set)
import Linear (V2(..))

import qualified Data.Set as Set



data World = World
  { playerPos :: Chn2 Int
  , chunk :: MapChunk
  } deriving (Eq, Show)

data MapChunk = MapChunk
  { treeRelPositions :: Set (Chn2 Int)
  } deriving (Eq, Show)

generateChunk :: V2 Int -> MapChunk
generateChunk pos = evalRand randomChunk (mkStdGen seed)
  where
    seed = hash pos

randomChunk :: MonadRandom m => m MapChunk
randomChunk = do
  trees <- filterM treeOnTile chunkRelPositions
  pure MapChunk { treeRelPositions = Set.fromList trees }
  where
    treeOnTile _tile = (< p) <$> getRandom
    p = 0.1 :: Float

chunkRelPositions :: Integral a => [Chn2 a]
chunkRelPositions = [Chn2 $ V2 x y
  | x <- [0 .. chunkSize - 1] , y <- [0 .. chunkSize - 1]]

chunkSize :: Num a => a
chunkSize = 16
