{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Spaces where

import Control.Lens (Lens', lens)
import Control.Lens.Operators
import Data.Hashable (Hashable)
import Data.Ix (Ix)
import Linear



-- |A vector in screen space
newtype Scr2 a = Scr2 { unScr2 :: V2 a }
  deriving (Functor, Applicative, Monad, Additive, Metric, Ix, Eq, Ord, Num,
    Hashable, Show)

instance R1 Scr2 where _x = lensScr2 . _x
instance R2 Scr2 where _y = lensScr2 . _y; _xy = lensScr2

-- |A vector in chunk space, i.e. tile coordinates relative to the origin of a
-- chunk
newtype Chn2 a = Chn2 { unChn2 :: V2 a }
  deriving (Functor, Applicative, Monad, Additive, Metric, Ix, Eq, Ord, Num,
    Hashable, Show)

instance R1 Chn2 where _x = lensChn2 . _x
instance R2 Chn2 where _y = lensChn2 . _y; _xy = lensChn2

newtype ChnIdx a = ChnIdx { unChnIdx :: V2 a }
  deriving (Functor, Applicative, Monad, Additive, Metric, Ix, Eq, Ord, Num,
    Hashable, Show)

instance R1 ChnIdx where _x = lensChnIdx . _x
instance R2 ChnIdx where _y = lensChnIdx . _y; _xy = lensChnIdx

tilesToScr :: (R2 t, Num (t a), Num a)
  => Scr2 a -> t a -> Scr2 a -> t a -> Scr2 a
tilesToScr tileSize eyeTiles eyeScr pos = eyeScr & _xy +~ eyeToPosOnScr
  where
    eyeToPosOnScr = (*) <$> unScr2 tileSize <*> (pos - eyeTiles) ^. _xy

scrToTiles :: (R2 t, Integral a) => Scr2 a -> Scr2 a -> t a -> Scr2 a -> t a
scrToTiles tileSize eyeScr eyeTiles pos = eyeTiles & _xy +~ eyeToPosInTiles
  where
    eyeToPosInTiles = div <$> unScr2 (pos - eyeScr) <*> unScr2 tileSize

normalizeChunkPos :: Integral a => ChnIdx a -> Chn2 a -> (ChnIdx a, Chn2 a)
normalizeChunkPos i pos = (i + di, pos')
  where
    di = ChnIdx . unChn2 $ div <$> pos <*> pure chunkSize
    pos' = mod <$> pos <*> pure chunkSize

chunkRelPositions :: Integral a => [Chn2 a]
chunkRelPositions = [Chn2 $ V2 x y
  | x <- [0 .. chunkSize - 1] , y <- [0 .. chunkSize - 1]]

chunkSize :: Num a => a
chunkSize = 16



lensScr2 :: Lens' (Scr2 a) (V2 a)
lensScr2 = lens unScr2 (const Scr2)

lensChn2 :: Lens' (Chn2 a) (V2 a)
lensChn2 = lens unChn2 (const Chn2)

lensChnIdx :: Lens' (ChnIdx a) (V2 a)
lensChnIdx = lens unChnIdx (const ChnIdx)
