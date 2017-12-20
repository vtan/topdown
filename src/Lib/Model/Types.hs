module Lib.Model.Types where

import Lib.Model.Spaces

import Data.Array (Array)
import Data.Map (Map)
import GHC.Generics (Generic)



data World = World
  { playerChunk :: ChunkV Int
  , playerPos :: InChunkV Int
  , chunkGlobals :: Array (ChunkV Int) ChunkGlobal
  , loadedChunkLocals :: Map (ChunkV Int) ChunkLocal
  , mapView :: MapView
  , inventory :: Map Object Int
  , activeDropdown :: Maybe Dropdown
  } deriving (Generic, Show)

data MapView
  = Global
  | Local
  deriving (Show)



data ChunkGlobal = ChunkGlobal
  { treeDensity :: Float
  , hasVillage :: Bool
  } deriving (Generic, Show)

data ChunkLocal = ChunkLocal
  { objects :: Map (InChunkV Int) [Object]
  } deriving (Generic, Show)

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
  { anchor :: InChunkV Double
  , commands :: [UserCommand]
  } deriving (Generic, Show)

data UserCommand
  = ShootArrow (InChunkV Int)
  | GetObject (ChunkV Int) (InChunkV Int) Object
  | TradeObjects Int Object Int Object
  deriving (Show)
