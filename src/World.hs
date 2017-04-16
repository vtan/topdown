{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module World where

import Spaces

import Control.Lens.TH
import Data.Map (Map)
import Data.Set (Set)
import Linear.V2



data World = World
  { worldPlayerChunk :: ChnIdx Int
  , worldPlayerPos :: Chn2 Int
  , worldChunkGlobals :: Map (ChnIdx Int) ChunkGlobal
  , worldChunkLocals :: Map (ChnIdx Int) ChunkLocal
  , worldMapView :: MapView
  } deriving (Eq, Show)

data MapView
  = Global
  | Local
  deriving (Eq, Show)

data ChunkGlobal = ChunkGlobal
  { chunkGlobalTreeDensity :: Float
  } deriving (Eq, Show)

data ChunkLocal = ChunkLocal
  { chunkLocalTrees :: Set (Chn2 Int)
  , chunkLocalArrows :: Set (Chn2 Int)
  } deriving (Eq, Show)

makeFields ''World
makeFields ''ChunkGlobal
makeFields ''ChunkLocal



screenSize :: Num a => Scr2 a
screenSize = Scr2 $ V2 800 600

tileSize :: Num a => Scr2 a
tileSize = Scr2 $ V2 32 32

playerEyeOnScr :: Scr2 Int
playerEyeOnScr = (`quot` 2) <$> screenSize - tileSize
