module Lib.Graphics.Resources where

import qualified Lib.Util as Util

import Control.Exception (IOException)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString

data Resources = Resources
  { font :: FilePath
  , images :: Map Text FilePath
  }
  deriving (Generic)

instance Aeson.FromJSON Resources

load :: FilePath -> IO (Either String Resources)
load path = do
  contentErr <- Util.tryShow @IOException $ ByteString.readFile path
  pure $ Aeson.eitherDecodeStrict =<< contentErr
