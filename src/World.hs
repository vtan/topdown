{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module World where

import Spaces

import Control.Arrow
import Control.Monad.Random
import Control.Monad.State
import Data.Hashable (hash)
import Data.Map (Map)
import Data.Set (Set)

import qualified Data.Map as Map
import qualified Data.Set as Set



data World = World
  { playerChunk :: ChnIdx Int
  , playerPos :: Chn2 Int
  , chunkGlobals :: Map (ChnIdx Int) ChunkGlobal
  , chunkLocals :: Map (ChnIdx Int) ChunkLocal
  , mapView :: MapView
  } deriving (Eq, Show)

data ChunkGlobal = ChunkGlobal
  { treeDensity :: Float
  } deriving (Eq, Show)

data ChunkLocal = ChunkLocal
  { treeRelPositions :: Set (Chn2 Int)
  } deriving (Eq, Show)

data MapView
  = Global
  | Local
  deriving (Eq, Show)

getChunkAt :: MonadState World m => ChnIdx Int -> m (ChunkGlobal, ChunkLocal)
getChunkAt i = do
  (global, globals) <- gets (insertIfAbsent i genGlobal . chunkGlobals)
  modify' $ \w -> w { chunkGlobals = globals }
  (local, locals) <- gets (insertIfAbsent i genLocal . chunkLocals)
  modify' $ \w -> w { chunkLocals = locals }
  pure (global, local)
  where
    (genGlobal, genLocal) = generateChunk i

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
    ( ChunkGlobal { treeDensity = density }
    , ChunkLocal { treeRelPositions = Set.fromList trees }
    )



insertIfAbsent :: Ord k => k -> a -> Map k a -> (a, Map k a)
insertIfAbsent k a = insert . (Map.lookup k &&& id)
  where
    insert = \case
      (Just a', m) -> (a', m)
      (Nothing, m) -> (a, Map.insert k a m)
