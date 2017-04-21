{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Chunk (Chunk, newChunk, chunkGlobal, getChunkLocal, addChunkLocalArrow)
  where

import ChunkData
import ChunkGen
import Spaces

import Control.Lens (_Just, contains, use)
import Control.Lens.Operators
import Control.Lens.TH (makeFields)
import Control.Monad (void)
import Control.Monad.State (MonadState)
import Data.Hashable (hash)



data Chunk = Chunk
  { chunkGlobal :: ChunkGlobal
  , chunkLocal :: Maybe ChunkLocal
  } deriving (Eq, Show)

newChunk :: ChunkGlobal -> Chunk
newChunk glob = Chunk
  { chunkGlobal = glob
  , chunkLocal = Nothing
  }

makeFields ''Chunk

getChunkLocal :: MonadState Chunk m => ChnIdx Int -> m ChunkLocal
getChunkLocal i = do
  maybeLoc <- use local
  case maybeLoc of
    Just loc -> pure loc
    Nothing -> do
      glob <- use global
      local <?= generateChunkLocal seed glob
  where
    seed = hash i

addChunkLocalArrow :: MonadState Chunk m => ChnIdx Int -> Chn2 Int -> m ()
addChunkLocalArrow i arrowPos = do
  void $ getChunkLocal i
  local . _Just . arrows . contains arrowPos .= True
