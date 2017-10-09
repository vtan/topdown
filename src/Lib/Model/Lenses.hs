{-# LANGUAGE TemplateHaskell #-}

module Lib.Model.Lenses where

import Lib.Model.Types
import Lib.TH



makeFields_ ''World
makeFields_ ''ChunkGlobal
makeFields_ ''ChunkLocal
