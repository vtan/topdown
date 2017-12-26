module Lib.Game.WorldGen (initialWorld) where

import Lib.Model.Spaces
import Lib.Game.World

import qualified Lib.Game.Object as Object

import Control.Monad (forM)
import Control.Monad.Random

import qualified Data.Array as Array
import qualified Data.Map as Map
import qualified Data.Ix as Ix



initialWorld :: MonadRandom m => m World
initialWorld = do
  assocs <- forM (Ix.range (0, worldSize - 1)) $ \i ->
    (i,) <$> generateChunkGlobal i
  pure World
    { playerChunk = 0
    , playerPos = 0
    , chunkGlobals = Array.array (0, worldSize - 1) assocs
    , loadedChunkLocals = mempty
    , mapView = Local
    , inventory = Map.fromList [(Object.Arrow, 10)]
    , activeDropdown = Nothing
    }



generateChunkGlobal :: MonadRandom m => ChunkV Int -> m ChunkGlobal
generateChunkGlobal chunk =
  ChunkGlobal <$> getRandom <*> hasVillage
  where
    hasVillage = all ((== 0) . (`mod` 10)) <$> perturbedChunk
    perturbedChunk = (chunk +) <$> (chunkV <$> d <*> d)
    d = getRandomR ((-3), 3)
