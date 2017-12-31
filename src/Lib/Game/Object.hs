module Lib.Game.Object where

data Object
  = Tree
  | Arrow
  | Deer
  | Meat
  | Wall
  | Villager
  | Gold
  deriving (Show, Eq, Ord)

blocksSight :: Object -> Bool
blocksSight = \case
  Tree -> True
  Wall -> True
  Arrow -> False
  Deer -> False
  Meat -> False
  Villager -> False
  Gold -> False
