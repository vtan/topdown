{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module World where

import Spaces

import Control.Lens.TH
import Data.Map (Map)
import Data.Set (Set)



data World = World
  { worldPlayerChunk :: ChnIdx Int
  , worldPlayerPos :: Chn2 Int
  , worldChunkGlobals :: Map (ChnIdx Int) ChunkGlobal
  , worldChunkLocals :: Map (ChnIdx Int) ChunkLocal
  , worldMapView :: MapView
  } deriving (Eq, Show)

data ChunkGlobal = ChunkGlobal
  { chunkGlobalTreeDensity :: Float
  } deriving (Eq, Show)

data ChunkLocal = ChunkLocal
  { chunkLocalTreeRelPositions :: Set (Chn2 Int)
  } deriving (Eq, Show)

data MapView
  = Global
  | Local
  deriving (Eq, Show)

makeFields ''World
makeFields ''ChunkGlobal
makeFields ''ChunkLocal
