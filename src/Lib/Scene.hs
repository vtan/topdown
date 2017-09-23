module Lib.Scene
  (Scene(..), Elem(..), Color(..), vmap, tileCenteredRectangle, render)
where

import Lib.Spaces
import Lib.Util

import Control.Lens
import Control.Monad.Zip
import Data.Foldable
import Data.Word
import Linear
import Linear.Affine (Point(P))
import SDL (($=))

import qualified SDL as Sdl



newtype Scene v a = Scene [Elem v a]
  deriving (Monoid, Eq, Show)

data Elem v a
  = Rectangle
    { _minCorner :: v a
    , _maxCorner :: v a
    , _color :: Color
    }
  deriving (Eq, Show)

data Color
  = Solid (V3 Word8)
  | Outline (V3 Word8)
  deriving (Eq, Show)



vmap :: (MonadZip v, Ord a) => (u a -> v a) -> Scene u a -> Scene v a
vmap f (Scene elems) = Scene $ map vmap' elems
  where
    vmap' (Rectangle minCorner maxCorner color) =
      let (minCorner', maxCorner') = minMaxZip (f minCorner) (f maxCorner)
      in Rectangle minCorner' maxCorner' color

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
    sdlRect mi ma = Just $ Sdl.Rectangle (P $ v2 mi) (v2 $ ma ^-^ mi)
    v2 = fmap floor . view _V2
