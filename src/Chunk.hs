{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Chunk where

import Spaces
import World
import WorldGen

import Control.Applicative
import Control.Lens
import Control.Monad.State



getChunkAt :: MonadState World m => ChnIdx Int -> m (ChunkGlobal, ChunkLocal)
getChunkAt i = do
  Just global <- chunkGlobals . at i <%= (<|> Just genGlobal)
  Just local <- chunkLocals . at i <%= (<|> Just genLocal)
  pure (global, local)
  where
    (genGlobal, genLocal) = generateChunk i

getPlayerChunk :: MonadState World m => m (ChunkGlobal, ChunkLocal)
getPlayerChunk = getChunkAt =<< use playerChunk
