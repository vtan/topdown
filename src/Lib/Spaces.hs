module Lib.Spaces where

import Control.Lens
import Data.Hashable (Hashable)
import Data.Ix (Ix)
import Linear



newtype ScreenV a = ScreenV { unScreenV :: V2 a }
  deriving (Functor, Applicative, Monad, Additive, Metric, Ix, Eq, Ord, Num,
    Hashable, Show)

instance R1 ScreenV where _x = _ScreenV . _x
instance R2 ScreenV where _y = _ScreenV . _y; _xy = _ScreenV

screenV :: a -> a -> ScreenV a
screenV x y = ScreenV $ V2 x y

newtype TileV a = TileV { unTileV :: V2 a }
  deriving (Functor, Applicative, Monad, Additive, Metric, Ix, Eq, Ord, Num,
    Hashable, Show)

instance R1 TileV where _x = _TileV . _x
instance R2 TileV where _y = _TileV . _y; _xy = _TileV

tileV :: a -> a -> TileV a
tileV x y = TileV $ V2 x y

newtype ChunkV a = ChunkV { unChunkV :: V2 a }
  deriving (Functor, Applicative, Monad, Additive, Metric, Ix, Eq, Ord, Num,
    Hashable, Show)

instance R1 ChunkV where _x = _ChunkV . _x
instance R2 ChunkV where _y = _ChunkV . _y; _xy = _ChunkV

chunkV :: a -> a -> ChunkV a
chunkV x y = ChunkV $ V2 x y

newtype InChunkV a = InChunkV { unInChunkV :: V2 a }
  deriving (Functor, Applicative, Monad, Additive, Metric, Ix, Eq, Ord, Num,
    Hashable, Show)

instance R1 InChunkV where _x = _InChunkV . _x
instance R2 InChunkV where _y = _InChunkV . _y; _xy = _InChunkV

inChunkV :: a -> a -> InChunkV a
inChunkV x y = InChunkV $ V2 x y



tilesToScr :: Num a => ScreenV a -> TileV a -> ScreenV a -> TileV a -> ScreenV a
tilesToScr tileSize eyeTiles eyeScr pos = eyeScr & _xy +~ eyeToPosOnScr
  where
    eyeToPosOnScr = (*) <$> unScreenV tileSize <*> (pos - eyeTiles) ^. _xy

scrToTiles :: Integral a => ScreenV a -> ScreenV a -> TileV a -> ScreenV a -> TileV a
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



_ScreenV :: Iso' (ScreenV a) (V2 a)
_ScreenV = iso unScreenV ScreenV

_TileV :: Iso' (TileV a) (V2 a)
_TileV = iso unTileV TileV

_InChunkV :: Iso' (InChunkV a) (V2 a)
_InChunkV = iso unInChunkV InChunkV

_ChunkV :: Iso' (ChunkV a) (V2 a)
_ChunkV = iso unChunkV ChunkV

tiledInChunk :: Iso' (InChunkV a) (TileV a)
tiledInChunk = _InChunkV . from _TileV

tiledChunk :: Iso' (ChunkV a) (TileV a)
tiledChunk = _ChunkV . from _TileV
