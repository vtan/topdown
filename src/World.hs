{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module World where

import ChunkData
import Spaces

import Control.Lens (Lens', lens)
import Control.Lens.TH (makeFields)
import Data.Array (Array, Ix)
import Data.Map (Map)
import Linear.V2

import qualified Data.Array as Array



data World = World
  { worldPlayerChunk :: ChnIdx Int
  , worldPlayerPos :: Chn2 Int
  , worldChunkGlobals :: Array (ChnIdx Int) ChunkGlobal
  , worldLoadedChunkLocals :: Map (ChnIdx Int) ChunkLocal
  , worldMapView :: MapView
  } deriving (Eq, Show)

data MapView
  = Global
  | Local
  deriving (Eq, Show)

makeFields ''World



validChunk :: (Num a, Ord a) => ChnIdx a -> Bool
validChunk i = xInRange && yInRange
  where
    ChnIdx (V2 xInRange yInRange) = (&&)
      <$> ((<=) <$> 0 <*> i)
      <*> ((<) <$> i <*> worldSize)

arrayAt :: Ix i => i -> Lens' (Array i a) a
arrayAt i = lens getter setter
  where
    getter a = a Array.! i
    setter a x = a Array.// [(i, x)]

worldSize :: Num a => ChnIdx a
worldSize = ChnIdx $ V2 20 20

screenSize :: Num a => Scr2 a
screenSize = Scr2 $ V2 800 600

tileSize :: Num a => Scr2 a
tileSize = Scr2 $ V2 32 32

playerEyeOnScr :: Scr2 Int
playerEyeOnScr = (`quot` 2) <$> screenSize - tileSize
