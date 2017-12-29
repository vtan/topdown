module Lib.Graphics.RenderContext where

import qualified Lib.Graphics.Resources as Resources
import qualified Lib.Util as Util

import Control.Lens
import Control.Monad.Except
import Data.Generics.Product (field)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified SDL as Sdl
import qualified SDL.Font as Sdl.Font
import qualified SDL.Image as Sdl.Image

data RenderContext = RenderContext
  { renderer :: Sdl.Renderer
  , font :: Sdl.Font.Font
  , images :: Map Text Sdl.Texture
  }
  deriving (Generic)

init :: FilePath -> Sdl.Renderer -> IO (Either String RenderContext)
init path renderer = runExceptT $ do
  resources <- ExceptT $ Resources.load path
  font <- Sdl.Font.load (resources ^. field @"font") 12
  images <-
    traverse (\imgPath ->
        ExceptT . Util.tryShow @Sdl.SDLException
          $ Sdl.Image.loadTexture renderer imgPath)
    $ resources ^. field @"images"
  pure RenderContext{ renderer, font, images }
