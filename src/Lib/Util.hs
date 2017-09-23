module Lib.Util where

import Control.Monad.Zip
import Data.Ix


minMax :: Ord a => a -> a -> (a, a)
minMax a b
  | a < b = (a, b)
  | otherwise = (b, a)

minMaxZip :: (MonadZip t, Ord a) => t a -> t a -> (t a, t a)
minMaxZip a b = munzip $ mzipWith minMax a b

rangeZip :: (MonadZip t, Ix (t a), Ord a) => (t a, t a) -> [t a]
rangeZip (x, y)= range $ minMaxZip x y
