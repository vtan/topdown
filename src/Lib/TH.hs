module Lib.TH where

import Control.Lens
import Data.Char (toLower)
import Data.Maybe (maybeToList)

import qualified Data.List as List
import qualified Language.Haskell.TH.Lib as TH
import qualified Language.Haskell.TH.Syntax as TH



makeFields_ :: TH.Name -> TH.DecsQ
makeFields_ =
  makeLensesWith $ set lensField fieldNamer defaultFieldRules

fieldNamer :: TH.Name -> [TH.Name] -> TH.Name -> [DefName]
fieldNamer (TH.nameBase -> ty) _fields (TH.nameBase -> field) =
  maybeToList $ do
    root <- List.stripPrefix (over _head toLower ty) field
    let cls = "Has" ++ root
        method = "_" ++ over _head toLower root
    Just $ MethodName (TH.mkName cls) (TH.mkName method)

