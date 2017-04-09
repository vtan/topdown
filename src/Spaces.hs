{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Spaces where

import Control.Lens
import Linear



-- |A vector in screen space
newtype Scr2 a = Scr2 { unScr2 :: V2 a }
  deriving (Functor, Additive, Eq, Ord, Num, Show)

instance R1 Scr2 where _x = lensScr2 . _x
instance R2 Scr2 where _y = lensScr2 . _y

-- |A vector in chunk space, i.e. tile coordinates relative to the origin of a
-- chunk
newtype Chn2 a = Chn2 { unChn2 :: V2 a }
  deriving (Functor, Additive, Eq, Ord, Num, Show)

instance R1 Chn2 where _x = lensChn2 . _x
instance R2 Chn2 where _y = lensChn2 . _y

chnToScr :: Num a => Scr2 a -> Chn2 a -> Scr2 a -> Chn2 a -> Scr2 a
chnToScr tileSize eyeChn eyeScr pos = eyeScr + eyeToPosOnScr
  where
    eyeToPosOnScr = Scr2 $ (*) <$> unScr2 tileSize <*> unChn2 (pos - eyeChn)

scrToChn :: Integral a => Scr2 a -> Scr2 a -> Chn2 a -> Scr2 a -> Chn2 a
scrToChn tileSize eyeScr eyeChn pos = eyeChn + eyeToPosInChn
  where
    eyeToPosInChn = Chn2 $ div <$> unScr2 (pos - eyeScr) <*> unScr2 tileSize

chunkRelPositions :: Integral a => [Chn2 a]
chunkRelPositions = [Chn2 $ V2 x y
  | x <- [0 .. chunkSize - 1] , y <- [0 .. chunkSize - 1]]

chunkSize :: Num a => a
chunkSize = 16



lensScr2 :: Lens' (Scr2 a) (V2 a)
lensScr2 = lens unScr2 (const Scr2)

lensChn2 :: Lens' (Chn2 a) (V2 a)
lensChn2 = lens unChn2 (const Chn2)
