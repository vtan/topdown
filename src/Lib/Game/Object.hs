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
