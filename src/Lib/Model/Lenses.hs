{-# LANGUAGE TemplateHaskell #-}

module Lib.Model.Lenses where

import Lib.Model.Types

import Control.Lens.TH



makeFields ''World
makeFields ''ChunkGlobal
makeFields ''ChunkLocal
