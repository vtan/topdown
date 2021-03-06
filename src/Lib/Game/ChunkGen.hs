module Lib.Game.ChunkGen where

import Lib.Game.Object (Object)
import Lib.Game.World (ChunkGlobal(..), ChunkLocal(..))
import Lib.Model.Spaces
import Lib.Util

import qualified Lib.Game.Object as Object

import Control.Lens
import Control.Monad (filterM)
import Control.Monad.Random (evalRand, mkStdGen)
import Data.Foldable
import Data.Generics.Product (field)
import Data.Map (Map)
import Data.Monoid
import Linear

import qualified Control.Monad.Random as Random
import qualified Data.Map as Map



generateChunkLocal :: Int -> ChunkGlobal -> ChunkLocal
generateChunkLocal seed global = flip evalRand (mkStdGen seed) $ do
  deerPos <- randomChance deerChance >>= \case
    True -> Just <$> Random.uniform chunkRelPositions
    False -> pure Nothing
  treePos <- filterM (const $ randomChance treeChance) chunkRelPositions
  villageMap <- case global ^. field @"hasVillage" of
    True -> generateVillage <$> Random.getRandomR (2, 5)
    False -> pure mempty
  let deers = map (, [Object.Deer]) $ toList deerPos
      trees = map (, [Object.Tree]) treePos
  pure ChunkLocal
    { objects =
        villageMap
        <> Map.fromList deers
        <> Map.fromList trees
    }
  where
    -- Per block in chunk
    treeChance = 0.2 * (global ^. field @"treeDensity")
    -- Per chunk
    deerChance = 0.7



generateVillage :: Int -> Map (InChunkV Int) [Object]
generateVillage houseCount =
  flip foldMap [1..houseCount] $ \i ->
    Map.fromList $ (mapped . _1 . _x +~ InChunk (i * 5)) house

house :: [(InChunkV Int, [Object])]
house =
  zip [ inChunkV 0 2, inChunkV 1 2, inChunkV 2 2
      , inChunkV 0 1, inChunkV 2 1
      , inChunkV 0 0, inChunkV 2 0
      ]
      (repeat [Object.Wall])
  ++ [(inChunkV 1 1, [Object.Villager]), (inChunkV 1 0, [])]
