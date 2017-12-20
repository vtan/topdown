module Lib.Model.World where

import Lib.Model.Spaces
import Lib.Model.Types

import Control.Lens
import Data.Char (toLower)
import Data.Generics.Product (field)
import Linear.V2



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

showObject :: Object -> String
showObject = over _head toLower . show

worldSize :: Num a => ChunkV a
worldSize = chunkV 100 100

tileSize :: Num a => ScreenV a
tileSize = screenV 32 32

dropdownItemSize :: Num a => ScreenV a
dropdownItemSize = screenV 180 16

playerEyeOnScr :: ScreenV Int
playerEyeOnScr = (`quot` 2) <$> screenSize - tileSize
