module Lib.Game.Dropdown
  (Dropdown(..), itemAt, render)
where

import Lib.Game.UserCommand (UserCommand)
import Lib.Graphics.Scene (Scene)
import Lib.Model.Spaces
import Lib.Util

import qualified Lib.Graphics.Scene as Scene

import Control.Lens
import Data.Monoid
import Data.Foldable
import Data.Text (Text)
import GHC.Generics (Generic)
import Linear

data Dropdown a = Dropdown
  { anchor :: a
  , commands :: [UserCommand]
  } deriving (Generic, Show, Functor)

itemAt :: (Num a, Ord a) => ScreenV a -> Dropdown (ScreenV a) -> Maybe UserCommand
itemAt pos dropdown =
  fmap snd
  . find (\(topLeft, _) -> inRectangle pos (topLeft, topLeft + itemSize))
  $ commandsWithTopLeft dropdown

render :: Num a => (UserCommand -> Text) -> Dropdown (ScreenV a) -> Scene (Screen a)
render showCommand dropdown =
  foldMap (\(topLeft, cmd) ->
    let bg = Scene.rectangle topLeft (topLeft + itemSize) bgStyle
        text = Scene.text topLeft (showCommand cmd) textColor
    in bg <> text)
  $ commandsWithTopLeft dropdown

commandsWithTopLeft :: Num a => Dropdown (ScreenV a) -> [(ScreenV a, UserCommand)]
commandsWithTopLeft Dropdown{ anchor, commands } =
  imap (\row cmd ->
    let pos = anchor + V2 0 (fromIntegral row *^ itemSize ^. _y)
    in (pos, cmd))
  $ commands

itemSize :: Num a => ScreenV a
itemSize = screenV 180 16

bgStyle :: Scene.Style
bgStyle = Scene.Solid 127

textColor :: Num a => V3 a
textColor = V3 255 255 255
