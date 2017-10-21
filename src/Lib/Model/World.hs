module Lib.Model.World where

import Lib.Model.Lenses
import Lib.Model.Spaces
import Lib.Model.Types

import Control.Lens
import Data.Char (toLower)
import Linear.V2



globalTileToScreen :: Num a => World -> ChunkV a -> ScreenV a
globalTileToScreen world =
  tilesToScr
    tileSize
    (fromIntegral <$> world ^. _playerChunk)
    (fromIntegral <$> playerEyeOnScr)

screenToGlobalTile :: World -> ScreenV Int -> ChunkV Int
screenToGlobalTile world = fmap (floor @Double)
  . scrToTiles tileSize playerEyeOnScr (world ^. _playerChunk)

localTileToScreen :: Num a => World -> InChunkV a -> ScreenV a
localTileToScreen world =
  tilesToScr
    tileSize
    (fromIntegral <$> world ^. _playerPos)
    (fromIntegral <$> playerEyeOnScr)

screenToLocalTile :: World -> ScreenV Int -> InChunkV Int
screenToLocalTile world = fmap (floor @Double)
  . scrToTiles tileSize playerEyeOnScr (world ^. _playerPos)

validChunk :: (Num a, Ord a) => ChunkV a -> Bool
validChunk i = xInRange && yInRange
  where
    ChunkV (V2 xInRange yInRange) = (&&)
      <$> ((<=) <$> 0 <*> i)
      <*> ((<) <$> i <*> worldSize)

objectsAt :: ChunkV Int -> InChunkV Int -> Traversal' World [Object]
objectsAt chunk pos =
  _loadedChunkLocals . at chunk . _Just . _objects . at pos . non []

objectsInInventory :: Object -> Lens' World Int
objectsInInventory obj = _inventory . at obj . non 0

showObject :: Object -> String
showObject = over _head toLower . show

worldSize :: Num a => ChunkV a
worldSize = chunkV 100 100

screenSize :: Num a => ScreenV a
screenSize = screenV 800 600

tileSize :: Num a => ScreenV a
tileSize = screenV 32 32

dropdownItemSize :: Num a => ScreenV a
dropdownItemSize = screenV 180 16

playerEyeOnScr :: ScreenV Int
playerEyeOnScr = (`quot` 2) <$> screenSize - tileSize
