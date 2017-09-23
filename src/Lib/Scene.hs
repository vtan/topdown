module Lib.Scene
  (Scene(..), Elem(..), Color(..), vmap, tileCenteredRectangle, render)
where

import Lib.Spaces

import Control.Lens
import Data.Foldable
import Data.Word
import Linear
import Linear.Affine (Point(P))
import SDL (($=))

import qualified SDL as Sdl



newtype Scene v a = Scene [Elem v a]
  deriving (Monoid)

data Elem v a
  = Rectangle
    { _minCorner :: v a
    , _maxCorner :: v a
    , _color :: Color
    }

data Color
  = Solid (V3 Word8)
  | Outline (V3 Word8)



vmap :: (u a -> v a) -> Scene u a -> Scene v a
vmap f (Scene elems) = Scene $ map vmap' elems
  where
    vmap' (Rectangle minCorner maxCorner color) =
      Rectangle (f minCorner) (f maxCorner) color

tileCenteredRectangle :: (IsTileV t, Fractional a)
  => t Int -> t a -> Color -> Scene t a
tileCenteredRectangle tile size color =
  Scene [Rectangle minCorner maxCorner color]
  where
    minCorner = fmap fromIntegral tile ^+^ (1 ^. from _V2 ^-^ size) ^/ 2
    maxCorner = minCorner ^+^ size

render :: RealFrac a => Sdl.Renderer -> Scene ScreenV a -> IO ()
render renderer (Scene elems) = traverse_ (renderElem renderer) elems

renderElem :: RealFrac a => Sdl.Renderer -> Elem ScreenV a -> IO ()
renderElem renderer (Rectangle minCorner maxCorner coloring) =
  case coloring of
    Solid color -> do
      setColor color
      Sdl.fillRect renderer $ sdlRect minCorner maxCorner
    Outline color -> do
      setColor color
      Sdl.drawRect renderer $ sdlRect minCorner maxCorner
  where
    setColor color = Sdl.rendererDrawColor renderer $= (_xyz .~ color $ 255)
    sdlRect (v2 -> mi) (v2 -> ma) = Just $ Sdl.Rectangle (P mi) (ma ^-^ mi)
    v2 = fmap floor . view _V2
