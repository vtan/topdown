module Lib.Graphics.Camera where

import Data.Coerce (Coercible, coerce)
import Linear

-- | A translate & scale transformation from a source space to a target space,
--   e.g. world space to screen space
data Camera a b = Camera
  { -- | The position of the camera in the target space
    eye :: V2 b
  , -- | The focus of the camera in the source space
    focus :: V2 a
  , -- | The size of the rectangle in the target space which corresponds
    --   to the unit square in the source space
    scale :: V2 b
  }
  deriving (Show)

project :: (Coercible a b, Num a, Num b) => Camera a b -> V2 a -> V2 b
project Camera{ eye, focus, scale } v =
  eye + scale * fmap coerce (v - focus)

invProject :: (Coercible b a, Fractional a, Num b) => Camera a b -> V2 b -> V2 a
invProject Camera{ eye, focus, scale } v =
  focus + fmap coerce (v - eye) / fmap coerce scale
