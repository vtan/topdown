module Lib.Util where

import Control.Monad.Random
import Control.Monad.Zip
import Data.Char (toLower)
import Data.Ix
import Linear


minMax :: Ord a => a -> a -> (a, a)
minMax a b
  | a < b = (a, b)
  | otherwise = (b, a)

minMaxZip :: (MonadZip t, Ord a) => t a -> t a -> (t a, t a)
minMaxZip a b = munzip $ mzipWith minMax a b

rangeZip :: (MonadZip t, Ix (t a), Ord a) => (t a, t a) -> [t a]
rangeZip (x, y)= range $ minMaxZip x y

withinRadius :: (Additive t, Foldable t, Integral a) => a -> t a -> t a -> Bool
withinRadius r x y = all (<= r) (abs <$> x ^-^ y)

inRectangle :: (Applicative t, Foldable t, Ord a) => t a -> (t a, t a) -> Bool
inRectangle p (minCorner, maxCorner) =
  all between $ (,,) <$> p <*> minCorner <*> maxCorner
  where
    between (x, mi, ma) = (x >= mi && x <= ma)



randomChance :: MonadRandom m => Float -> m Bool
randomChance x = (< x) <$> getRandom



newtype Lower a = Lower { unLower :: a }
instance Show a => Show (Lower a) where
  show = map toLower . show . unLower
