module Lib.Model.Line (line) where

import Linear

line :: Integral a => V2 a -> V2 a -> [V2 a]
line u v = map (u +) $ lineFromOrigin (v - u)

-- Based on http://tech-algorithm.com/articles/drawing-line-using-bresenham-algorithm/
lineFromOrigin :: forall a. Integral a => V2 a -> [V2 a]
lineFromOrigin (V2 dx dy) =
  take (fromIntegral longest + 1) . map snd
  $ iterate next (longest `quot` 2, 0)
  where
    next :: (a, V2 a) -> (a, V2 a)
    next (numerator, V2 x y) =
      let numerator' = numerator + shortest
       in if numerator' < longest
          then (numerator', V2 (x + dx2) (y + dy2))
          else (numerator' - longest, V2 (x + dx1) (y + dy1))

    adx = abs dx
    ady = abs dy
    dx1 = signum dx
    dy1 = signum dy
    (dx2, dy2, longest, shortest)
      | adx > ady = (signum dx, 0, adx, ady)
      | otherwise = (0, signum dy, ady, adx)
