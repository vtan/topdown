module Lib.Util where

import Control.Monad.Random
import Control.Monad.Zip
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

neighborOf :: (Additive t, Foldable t, Integral a) => t a -> t a -> Bool
neighborOf x y = all (<= 1) absDiff && any (/= 0) absDiff
  where
    absDiff = abs <$> x ^-^ y



randomChance :: MonadRandom m => Float -> m Bool
randomChance x = (< x) <$> getRandom
