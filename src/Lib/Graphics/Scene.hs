module Lib.Graphics.Scene
  ( Scene(..), Elem(..), Style(..)
  , vmap, rectangle, tileCenteredRectangle, text
  , render
  )
where

import Lib.Model.Spaces
import Lib.Util

import Control.Lens
import Data.Foldable
import Data.Text (Text)
import Data.Word
import Linear
import Linear.Affine (Point(P))
import SDL (($=))

import qualified SDL as Sdl
import qualified SDL.Font as Sdl.Font



newtype Scene v a = Scene [Elem v a]
  deriving (Functor, Monoid, Eq, Show)

data Elem v a
  = Rectangle
    { _minCorner :: v a
    , _maxCorner :: v a
    , _style :: Style
    }
  | Text
    { _corner :: v a
    , _text :: Text
    , _color :: V3 Word8
    }
  deriving (Functor, Eq, Show)

data Style
  = Solid (V3 Word8)
  | Outline (V3 Word8)
  deriving (Eq, Show)



vmap :: Ord b => (V2 a -> V2 b) -> Scene V2 a -> Scene V2 b
vmap f (Scene elems) = Scene $ map vmap' elems
  where
    vmap' = \case
      Rectangle minCorner maxCorner style ->
        let (minCorner', maxCorner') = minMaxZip (f minCorner) (f maxCorner)
        in Rectangle minCorner' maxCorner' style
      Text corner size text_ ->
        Text (f corner) size text_

rectangle :: t a -> t a -> Style -> Scene t a
rectangle mi ma s = Scene [Rectangle mi ma s]

tileCenteredRectangle :: (Integral a, Fractional b) => V2 a -> V2 b -> Style -> Scene V2 b
tileCenteredRectangle tile size style =
  Scene [Rectangle minCorner maxCorner style]
  where
    minCorner = fmap fromIntegral tile + (V2 1 1 - size) ^/ 2
    maxCorner = minCorner + size

text :: v a -> Text -> V3 Word8 -> Scene v a
text p t c = Scene [Text p t c]

render :: RealFrac a
  => Sdl.Renderer -> Sdl.Font.Font -> Scene V2 (Screen a) -> IO ()
render renderer font (Scene elems) = traverse_ (renderElem renderer font) elems

renderElem :: RealFrac a
  => Sdl.Renderer -> Sdl.Font.Font -> Elem V2 (Screen a) -> IO ()
renderElem renderer font = \case
  Rectangle minCorner maxCorner style -> case style of
    Solid color -> do
      setColor color
      Sdl.fillRect renderer $ sdlRect minCorner maxCorner
    Outline color -> do
      setColor color
      Sdl.drawRect renderer $ sdlRect minCorner maxCorner
  Text corner text_ (color4 -> color) -> do
    surface <- Sdl.Font.blended font color text_
    size <- Sdl.surfaceDimensions surface
    texture <- Sdl.createTextureFromSurface renderer surface
    Sdl.copy renderer texture Nothing (Just $ Sdl.Rectangle (P $ v2 corner) size)
    Sdl.destroyTexture texture
    Sdl.freeSurface surface
  where
    color4 color = _xyz .~ color $ 255
    setColor (color4 -> color) = Sdl.rendererDrawColor renderer $= color
    sdlRect mi ma = Just $ Sdl.Rectangle (P $ v2 mi) (v2 $ ma - mi)
    v2 = fmap (floor . unScreen)
