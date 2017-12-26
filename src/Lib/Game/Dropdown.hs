module Lib.Game.Dropdown where

import Lib.Game.UserCommand (UserCommand)
import Lib.Model.Spaces

import GHC.Generics (Generic)

data Dropdown = Dropdown
  { anchor :: InChunkV Double
  , commands :: [UserCommand]
  } deriving (Generic, Show)
