module Lib.Spaces where

import Control.Lens (Lens', lens)
import Control.Lens.Operators
import Data.Hashable (Hashable)
import Data.Ix (Ix)
import Linear



newtype ScreenV a = ScreenV { unScreenV :: V2 a }
  deriving (Functor, Applicative, Monad, Additive, Metric, Ix, Eq, Ord, Num,
    Hashable, Show)

instance R1 ScreenV where _x = lensScreenV . _x
instance R2 ScreenV where _y = lensScreenV . _y; _xy = lensScreenV

screenV :: a -> a -> ScreenV a
screenV x y = ScreenV $ V2 x y

newtype ChunkV a = ChunkV { unChunkV :: V2 a }
  deriving (Functor, Applicative, Monad, Additive, Metric, Ix, Eq, Ord, Num,
    Hashable, Show)

instance R1 ChunkV where _x = lensChunkV . _x
instance R2 ChunkV where _y = lensChunkV . _y; _xy = lensChunkV

chunkV :: a -> a -> ChunkV a
chunkV x y = ChunkV $ V2 x y

newtype InChunkV a = InChunkV { unInChunkV :: V2 a }
  deriving (Functor, Applicative, Monad, Additive, Metric, Ix, Eq, Ord, Num,
    Hashable, Show)

instance R1 InChunkV where _x = lensInChunkV . _x
instance R2 InChunkV where _y = lensInChunkV . _y; _xy = lensInChunkV

inChunkV :: a -> a -> InChunkV a
inChunkV x y = InChunkV $ V2 x y



tilesToScr :: (R2 t, Num (t a), Num a)
  => ScreenV a -> t a -> ScreenV a -> t a -> ScreenV a
tilesToScr tileSize eyeTiles eyeScr pos = eyeScr & _xy +~ eyeToPosOnScr
  where
    eyeToPosOnScr = (*) <$> unScreenV tileSize <*> (pos - eyeTiles) ^. _xy

scrToTiles :: (R2 t, Integral a) => ScreenV a -> ScreenV a -> t a -> ScreenV a -> t a
scrToTiles tileSize eyeScr eyeTiles pos = eyeTiles & _xy +~ eyeToPosInTiles
  where
    eyeToPosInTiles = div <$> unScreenV (pos - eyeScr) <*> unScreenV tileSize

normalizeChunkPos :: Integral a => ChunkV a -> InChunkV a -> (ChunkV a, InChunkV a)
normalizeChunkPos i pos = (i + di, pos')
  where
    di = ChunkV . unInChunkV $ div <$> pos <*> pure chunkSize
    pos' = mod <$> pos <*> pure chunkSize

chunkRelPositions :: Integral a => [InChunkV a]
chunkRelPositions = [InChunkV $ V2 x y
  | x <- [0 .. chunkSize - 1] , y <- [0 .. chunkSize - 1]]

chunkSize :: Num a => a
chunkSize = 16



lensScreenV :: Lens' (ScreenV a) (V2 a)
lensScreenV = lens unScreenV (const ScreenV)

lensInChunkV :: Lens' (InChunkV a) (V2 a)
lensInChunkV = lens unInChunkV (const InChunkV)

lensChunkV :: Lens' (ChunkV a) (V2 a)
lensChunkV = lens unChunkV (const ChunkV)
