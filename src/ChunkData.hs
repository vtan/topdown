{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module ChunkData where

import Spaces

import Control.Lens.TH (makeFields)
import Data.Map (Map)



data ChunkGlobal = ChunkGlobal
  { chunkGlobalTreeDensity :: Float
  } deriving (Eq, Show)

data ChunkLocal = ChunkLocal
  { chunkLocalObjects :: Map (Chn2 Int) [Object]
  } deriving (Eq, Show)

data Object
  = Tree
  | Arrow
  deriving (Eq, Show)

makeFields ''ChunkGlobal
makeFields ''ChunkLocal
