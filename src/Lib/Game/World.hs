module Lib.Game.World where

import Lib.Game.Dropdown (Dropdown)
import Lib.Game.Object (Object)
import Lib.Model.Spaces

import Control.Lens
import Data.Array (Array)
import Data.Char (toLower)
import Data.Generics.Product (field)
import Data.Map (Map)
import GHC.Generics (Generic)
import Linear.V2



data World = World
  { playerChunk :: ChunkV Int
  , playerPos :: InChunkV Int
  , chunkGlobals :: Array (ChunkV Int) ChunkGlobal
  , loadedChunkLocals :: Map (ChunkV Int) ChunkLocal
  , mapView :: MapView
  , inventory :: Map Object Int
  , activeDropdown :: Maybe Dropdown
  } deriving (Generic, Show)

data ChunkGlobal = ChunkGlobal
  { treeDensity :: Float
  , hasVillage :: Bool
  } deriving (Generic, Show)

data ChunkLocal = ChunkLocal
  { objects :: Map (InChunkV Int) [Object]
  } deriving (Generic, Show)

data MapView
  = Global
  | Local
  deriving (Show)



globalTileToScreen :: Num a => World -> ChunkV a -> ScreenV a
globalTileToScreen world =
  tilesToScr
    tileSize
    (fromIntegral <$> world ^. field @"playerChunk")
    (fromIntegral <$> playerEyeOnScr)
  . fmap (Tile . unChunk)

screenToGlobalTile :: World -> ScreenV Int -> ChunkV Int
screenToGlobalTile world = fmap (floor @(Tile Double))
  . scrToTiles tileSize playerEyeOnScr (fromIntegral <$> world ^. field @"playerChunk")

localTileToScreen :: Num a => World -> InChunkV a -> ScreenV a
localTileToScreen world =
  tilesToScr
    tileSize
    (fromIntegral <$> world ^. field @"playerPos")
    (fromIntegral <$> playerEyeOnScr)
  . fmap (Tile . unInChunk)

screenToLocalTile :: World -> ScreenV Int -> InChunkV Int
screenToLocalTile world = fmap (floor @(Tile Double))
  . scrToTiles tileSize playerEyeOnScr (fromIntegral <$> world ^. field @"playerPos")

validChunk :: (Num a, Ord a) => ChunkV a -> Bool
validChunk i = xInRange && yInRange
  where
    V2 xInRange yInRange = (&&)
      <$> ((<=) <$> 0 <*> i)
      <*> ((<) <$> i <*> worldSize)

objectsAt :: ChunkV Int -> InChunkV Int -> Traversal' World [Object]
objectsAt chunk pos =
  field @"loadedChunkLocals" . at chunk . _Just . field @"objects" . at pos . non []

objectsInInventory :: Object -> Lens' World Int
objectsInInventory obj = field @"inventory" . at obj . non 0

worldSize :: Num a => ChunkV a
worldSize = chunkV 100 100

tileSize :: Num a => ScreenV a
tileSize = screenV 32 32

dropdownItemSize :: Num a => ScreenV a
dropdownItemSize = screenV 180 16

playerEyeOnScr :: ScreenV Int
playerEyeOnScr = (`quot` 2) <$> screenSize - tileSize
