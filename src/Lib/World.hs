{-# LANGUAGE TemplateHaskell #-}

module Lib.World where

import Lib.ChunkData
import Lib.Spaces

import Control.Lens
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
  } deriving (Eq, Show)

data MapView
  = Global
  | Local
  deriving (Eq, Show)

makeFields ''World



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
worldSize = chunkV 20 20

screenSize :: Num a => ScreenV a
screenSize = screenV 800 600

tileSize :: Num a => ScreenV a
tileSize = screenV 32 32

playerEyeOnScr :: ScreenV Int
playerEyeOnScr = (`quot` 2) <$> screenSize - tileSize
