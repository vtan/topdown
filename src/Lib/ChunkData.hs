{-# LANGUAGE TemplateHaskell #-}

module Lib.ChunkData where

import Lib.Spaces

import Control.Lens
import Data.Char (toLower)
import Data.Map (Map)



data ChunkGlobal = ChunkGlobal
  { chunkGlobalTreeDensity :: Float
  , chunkGlobalHasVillage :: Bool
  } deriving (Eq, Show)

data ChunkLocal = ChunkLocal
  { chunkLocalObjects :: Map (InChunkV Int) [Object]
  } deriving (Eq, Show)

data Object
  = Tree
  | Arrow
  | Deer
  | Meat
  | Wall
  | Villager
  | Gold
  deriving (Eq, Ord, Show)

showObject :: Object -> String
showObject = over _head toLower . show

makeFields ''ChunkGlobal
makeFields ''ChunkLocal
