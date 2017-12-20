module Lib.Model.Spaces where

import Control.Lens
import Data.Hashable (Hashable)
import Data.Ix (Ix)
import Linear



newtype Screen a = Screen { unScreen :: a }
  deriving (Show, Functor, Eq, Ord, Enum, Num, Integral, Fractional, Real, RealFrac, Hashable, Ix)

newtype Tile a = Tile { unTile :: a }
  deriving (Show, Functor, Eq, Ord, Enum, Num, Integral, Fractional, Real, RealFrac, Hashable, Ix)

newtype Chunk a = Chunk { unChunk :: a }
  deriving (Show, Functor, Eq, Ord, Enum, Num, Integral, Fractional, Real, RealFrac, Hashable, Ix)

newtype InChunk a = InChunk { unInChunk :: a }
  deriving (Show, Functor, Eq, Ord, Enum, Num, Integral, Fractional, Real, RealFrac, Hashable, Ix)

type ScreenV a = V2 (Screen a)
type TileV a = V2 (Tile a)
type ChunkV a = V2 (Chunk a)
type InChunkV a = V2 (InChunk a)

screenV :: a -> a -> V2 (Screen a)
screenV x y = V2 (Screen x) (Screen y)

tileV :: a -> a -> V2 (Tile a)
tileV x y = V2 (Tile x) (Tile y)

chunkV :: a -> a -> V2 (Chunk a)
chunkV x y = V2 (Chunk x) (Chunk y)

inChunkV :: a -> a -> V2 (InChunk a)
inChunkV x y = V2 (InChunk x) (InChunk y)



tilesToScr :: Num a => V2 (Screen a) -> V2 (Tile a) -> V2 (Screen a) -> V2 (Tile a) -> V2 (Screen a)
tilesToScr tileSize eyeTiles eyeScr pos = eyeScr + eyeToPosOnScr
  where
    eyeToPosOnScr = Screen <$>
      over _y negate (fmap unScreen tileSize * fmap unTile (pos - eyeTiles))

scrToTiles :: (Integral a, Fractional b)
  => V2 (Screen a) -> V2 (Screen a) -> V2 (Tile a) -> V2 (Screen a) -> V2 (Tile b)
scrToTiles tileSize eyeScr eyeTiles pos =
  fmap fromIntegral eyeTiles + eyeToPosInTiles
  where
    eyeToPosInTiles = Tile . unScreen <$>
      (over _y negate . fmap fromIntegral $ pos - eyeScr) / fmap fromIntegral tileSize

normalizeChunkPos :: Integral a => ChunkV a -> InChunkV a -> (ChunkV a, InChunkV a)
normalizeChunkPos i pos = (i + di, pos')
  where
    di = Chunk . unInChunk <$>
      (div <$> pos <*> chunkSize)
    pos' = mod <$> pos <*> pure chunkSize

chunkRelPositions :: Integral a => [InChunkV a]
chunkRelPositions = [inChunkV x y
  | x <- [0 .. chunkSize - 1] , y <- [0 .. chunkSize - 1]]

chunkSize :: Num a => a
chunkSize = 64

screenSize :: Num a => ScreenV a
screenSize = screenV 1280 720
