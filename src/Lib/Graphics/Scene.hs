module Lib.Graphics.Scene
  ( Scene(..), Elem(..), Style(..)
  , vmap, rectangle, tileCenteredRectangle, image, tileCenteredImage, text
  , render
  )
where

import Lib.Graphics.RenderContext (RenderContext(..))
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



newtype Scene a = Scene [Elem a]
  deriving (Functor, Monoid, Eq, Show)

data Elem a
  = Rectangle
    { _minCorner :: V2 a
    , _maxCorner :: V2 a
    , _style :: Style
    }
  | Image
    { _name :: Text
    , _minCorner :: V2 a
    , _maxCorner :: V2 a
    }
  | Text
    { _corner :: V2 a
    , _text :: Text
    , _color :: V4 Word8
    }
  deriving (Functor, Eq, Show)

data Style
  = Solid (V4 Word8)
  | Outline (V4 Word8)
  deriving (Eq, Show)



vmap :: Ord b => (V2 a -> V2 b) -> Scene a -> Scene b
vmap f (Scene elems) = Scene $ map vmap' elems
  where
    vmap' = \case
      Rectangle minCorner maxCorner style ->
        let (minCorner', maxCorner') = minMaxZip (f minCorner) (f maxCorner)
        in Rectangle minCorner' maxCorner' style
      Image name minCorner maxCorner ->
        let (minCorner', maxCorner') = minMaxZip (f minCorner) (f maxCorner)
        in Image name minCorner' maxCorner'
      Text corner size text_ ->
        Text (f corner) size text_

rectangle :: V2 a -> V2 a -> Style -> Scene a
rectangle mi ma s = Scene [Rectangle mi ma s]

tileCenteredRectangle :: (Integral a, Fractional b) => V2 a -> V2 b -> Style -> Scene b
tileCenteredRectangle tile size style =
  Scene [Rectangle minCorner maxCorner style]
  where
    minCorner = fmap fromIntegral tile + (V2 1 1 - size) ^/ 2
    maxCorner = minCorner + size

image :: Text -> V2 a -> V2 a -> Scene a
image n mi ma = Scene [Image n mi ma]

tileCenteredImage :: (Integral a, Fractional b) => Text -> V2 a -> V2 b -> Scene b
tileCenteredImage name tile size =
  Scene [Image name minCorner maxCorner]
  where
    minCorner = fmap fromIntegral tile + (V2 1 1 - size) ^/ 2
    maxCorner = minCorner + size

text :: V2 a -> Text -> V4 Word8 -> Scene a
text p t c = Scene [Text p t c]

render :: RealFrac a => RenderContext -> Scene (Screen a) -> IO ()
render renderCtx (Scene elems) =
  traverse_ (renderElem renderCtx) elems

renderElem :: RealFrac a => RenderContext -> Elem (Screen a) -> IO ()
renderElem RenderContext{ renderer, font, images } = \case
  Rectangle minCorner maxCorner style -> case style of
    Solid color -> do
      setColor color
      Sdl.fillRect renderer $ sdlRect minCorner maxCorner
    Outline color -> do
      setColor color
      Sdl.drawRect renderer $ sdlRect minCorner maxCorner
  Image name minCorner maxCorner ->
    for_ (images ^. at name) $ \texture ->
      Sdl.copy renderer texture Nothing $ sdlRect minCorner maxCorner
  Text corner text_ color -> do
    surface <- Sdl.Font.blended font color text_
    size <- Sdl.surfaceDimensions surface
    texture <- Sdl.createTextureFromSurface renderer surface
    Sdl.copy renderer texture Nothing (Just $ Sdl.Rectangle (P $ v2 corner) size)
    Sdl.destroyTexture texture
    Sdl.freeSurface surface
  where
    setColor color = Sdl.rendererDrawColor renderer $= color
    sdlRect mi ma = Just $ Sdl.Rectangle (P $ v2 mi) (v2 $ ma - mi)
    v2 = fmap (floor . unScreen)
