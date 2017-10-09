module Lib.Model.Types where

import Lib.Model.Spaces

import Data.Array (Array)
import Data.Map (Map)



data World = World
  { worldPlayerChunk :: ChunkV Int
  , worldPlayerPos :: InChunkV Int
  , worldChunkGlobals :: Array (ChunkV Int) ChunkGlobal
  , worldLoadedChunkLocals :: Map (ChunkV Int) ChunkLocal
  , worldMapView :: MapView
  , worldInventory :: Map Object Int
  , worldActiveDropdown :: Maybe Dropdown
  } deriving (Show)

data MapView
  = Global
  | Local
  deriving (Show)



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



data Dropdown = Dropdown
  { dropdownAnchor :: InChunkV Double
  , dropdownCommands :: [UserCommand]
  } deriving (Show)

data UserCommand
  = ShootArrow (InChunkV Int)
  | GetObject (ChunkV Int) (InChunkV Int) Object
  | TradeObjects Int Object Int Object
  deriving (Show)
