module Lib.Spaces where

import Control.Lens
import Control.Monad.Zip
import Data.Hashable (Hashable)
import Data.Ix (Ix)
import Linear



newtype ScreenV a = ScreenV { unScreenV :: V2 a }
  deriving (Functor, Applicative, Monad, Foldable, MonadZip,
    Additive, Metric, Ix, Eq, Ord, Num, Fractional, Hashable, Show)

screenV :: a -> a -> ScreenV a
screenV x y = ScreenV $ V2 x y

newtype TileV a = TileV { unTileV :: V2 a }
  deriving (Functor, Applicative, Monad, Foldable, MonadZip,
    Additive, Metric, Ix, Eq, Ord, Num, Fractional, Hashable, Show)

tileV :: a -> a -> TileV a
tileV x y = TileV $ V2 x y

newtype ChunkV a = ChunkV { unChunkV :: V2 a }
  deriving (Functor, Applicative, Monad, Foldable, MonadZip,
    Additive, Metric, Ix, Eq, Ord, Num, Fractional, Hashable, Show)

chunkV :: a -> a -> ChunkV a
chunkV x y = ChunkV $ V2 x y

newtype InChunkV a = InChunkV { unInChunkV :: V2 a }
  deriving (Functor, Applicative, Monad, Foldable, MonadZip,
    Additive, Metric, Ix, Eq, Ord, Num, Fractional, Hashable, Show)

inChunkV :: a -> a -> InChunkV a
inChunkV x y = InChunkV $ V2 x y



class (Additive t, R2 t) => IsV2 t where
  _V2 :: Iso (t a) (t b) (V2 a) (V2 b)

instance R1 ScreenV where _x = _V2 . _x
instance R2 ScreenV where _y = _V2 . _y; _xy = _V2
instance IsV2 ScreenV where _V2 = iso unScreenV ScreenV

instance R1 TileV where _x = _V2 . _x
instance R2 TileV where _y = _V2 . _y; _xy = _V2
instance IsV2 TileV where _V2 = iso unTileV TileV

instance R1 ChunkV where _x = _V2 . _x
instance R2 ChunkV where _y = _V2 . _y; _xy = _V2
instance IsV2 ChunkV where _V2 = iso unChunkV ChunkV

instance R1 InChunkV where _x = _V2 . _x
instance R2 InChunkV where _y = _V2 . _y; _xy = _V2
instance IsV2 InChunkV where _V2 = iso unInChunkV InChunkV

class IsV2 t => IsTileV t where
  _TileV :: Iso (t a) (t b) (TileV a) (TileV b)

instance IsTileV TileV where _TileV = id
instance IsTileV ChunkV where _TileV = _V2 . from _V2
instance IsTileV InChunkV where _TileV = _V2 . from _V2



tilesToScr :: (IsTileV t, Num a) => ScreenV a -> t a -> ScreenV a -> t a -> ScreenV a
tilesToScr tileSize eyeTiles eyeScr pos = eyeScr + eyeToPosOnScr
  where
    eyeToPosOnScr = view (from _V2) $
      over _y negate $ view _V2 tileSize * view _V2 (pos ^-^ eyeTiles)

scrToTiles :: (IsTileV t, Integral a, Fractional b)
  => ScreenV a -> ScreenV a -> t a -> ScreenV a -> t b
scrToTiles tileSize eyeScr eyeTiles pos =
  fmap fromIntegral eyeTiles ^+^ eyeToPosInTiles
  where
    eyeToPosInTiles = view (_V2 . from _V2) $
      (over _y negate . fmap fromIntegral $ pos - eyeScr) / fmap fromIntegral tileSize

normalizeChunkPos :: Integral a => ChunkV a -> InChunkV a -> (ChunkV a, InChunkV a)
normalizeChunkPos i pos = (i + di, pos')
  where
    di = view (_V2 . from _V2) $
      div <$> pos <*> chunkSize
    pos' = mod <$> pos <*> pure chunkSize

chunkRelPositions :: Integral a => [InChunkV a]
chunkRelPositions = [InChunkV $ V2 x y
  | x <- [0 .. chunkSize - 1] , y <- [0 .. chunkSize - 1]]

chunkSize :: Num a => a
chunkSize = 16
