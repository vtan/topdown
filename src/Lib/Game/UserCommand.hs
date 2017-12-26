module Lib.Game.UserCommand where

import Lib.Game.Object (Object)
import Lib.Model.Spaces

data UserCommand
  = ShootArrow (InChunkV Int)
  | GetObject (ChunkV Int) (InChunkV Int) Object
  | TradeObjects Int Object Int Object
  deriving (Show)
