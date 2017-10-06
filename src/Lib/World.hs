{-# LANGUAGE TemplateHaskell #-}

module Lib.World where

import Lib.ChunkData
import Lib.Spaces

import Control.Lens
import Control.Monad.Random
import Data.Array (Array, Ix)
import Data.Map (Map)
import Linear.V2

import qualified Data.Array as Array



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

data Dropdown = Dropdown
  { dropdownAnchor :: InChunkV Double
  , dropdownItems :: [DropdownItem]
  } deriving (Show)

data DropdownItem = DropdownItem
  { dropdownItemCommand :: UserCommand
  , dropdownItemCont :: forall m. MonadRandom m => World -> m World
  }

instance Show DropdownItem where show (DropdownItem cmd _) = show cmd

data UserCommand
  = ShootArrow
  | GetObject Object
  | TradeObject Int Object Int Object
  deriving (Show)

makeFields ''World



globalTileToScreen :: Num a => World -> ChunkV a -> ScreenV a
globalTileToScreen world =
  tilesToScr
    tileSize
    (fromIntegral <$> world ^. playerChunk)
    (fromIntegral <$> playerEyeOnScr)

screenToGlobalTile :: World -> ScreenV Int -> ChunkV Int
screenToGlobalTile world = fmap (floor @Double)
  . scrToTiles tileSize playerEyeOnScr (world ^. playerChunk)

localTileToScreen :: Num a => World -> InChunkV a -> ScreenV a
localTileToScreen world =
  tilesToScr
    tileSize
    (fromIntegral <$> world ^. playerPos)
    (fromIntegral <$> playerEyeOnScr)

screenToLocalTile :: World -> ScreenV Int -> InChunkV Int
screenToLocalTile world = fmap (floor @Double)
  . scrToTiles tileSize playerEyeOnScr (world ^. playerPos)

validChunk :: (Num a, Ord a) => ChunkV a -> Bool
validChunk i = xInRange && yInRange
  where
    ChunkV (V2 xInRange yInRange) = (&&)
      <$> ((<=) <$> 0 <*> i)
      <*> ((<) <$> i <*> worldSize)

arrayAt :: Ix i => i -> Lens' (Array i a) a
arrayAt i = lens getter setter
  where
    getter a = a Array.! i
    setter a x = a Array.// [(i, x)]

objectsAt :: ChunkV Int -> InChunkV Int -> Traversal' World [Object]
objectsAt chunk pos =
  loadedChunkLocals . at chunk . _Just . objects . at pos . non []

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
