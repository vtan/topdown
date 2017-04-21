{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module ChunkData where

import Spaces

import Control.Lens.TH (makeFields)
import Data.Set (Set)



data ChunkGlobal = ChunkGlobal
  { chunkGlobalTreeDensity :: Float
  } deriving (Eq, Show)

data ChunkLocal = ChunkLocal
  { chunkLocalTrees :: Set (Chn2 Int)
  , chunkLocalArrows :: Set (Chn2 Int)
  } deriving (Eq, Show)

makeFields ''ChunkGlobal
makeFields ''ChunkLocal
