module Lib.Graphics.RenderContext where

import qualified SDL as Sdl
import qualified SDL.Font as Sdl.Font

data RenderContext = RenderContext
  { renderer :: Sdl.Renderer
  , font :: Sdl.Font.Font
  }
  deriving (Show)
