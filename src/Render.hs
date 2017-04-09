module Render where

import Spaces
import World

import Control.Monad
import Control.Monad.State
import Data.Foldable (for_)
import SDL

import qualified Data.Set as Set



renderWorld :: Renderer -> World -> IO World
renderWorld renderer world = do
  rendererDrawColor renderer $= bgColor
  clear renderer

  let
    Chn2 (V2 left top) = chn 0
    Chn2 (V2 right bottom) = chn screenSize
    tiles = [Chn2 $ V2 x y
      | x <- [left .. right]
      , y <- [top .. bottom]
      ]
  world' <- flip execStateT world $
    for_ tiles $ \pos -> do
      let (i, pos') = normalizeChunkPos (playerChunk world) pos
      chunk <- state $ getChunkAt i
      rendererDrawColor renderer $= terrainColor
      fillRect renderer . Just $ rect pos
      when (Set.member pos' $ treeRelPositions chunk) $ do
        rendererDrawColor renderer $= treeColor
        fillRect renderer . Just $ rect pos

  rendererDrawColor renderer $= playerColor
  fillRect renderer . Just $ rect (playerPos world)

  present renderer
  pure world'
  where
    rect p = fromIntegral
      <$> Rectangle (P . unScr2 $ scr p - halfTile) (unScr2 tileSize)
    scr = chnToScr tileSize (playerPos world) midScr
    chn = scrToChn tileSize midScr (playerPos world)
    halfTile = (`quot` 2) <$> tileSize
    midScr = (`quot` 2) <$> screenSize



screenSize :: Num a => Scr2 a
screenSize = Scr2 $ V2 800 600

tileSize :: Num a => Scr2 a
tileSize = Scr2 $ V2 32 32

bgColor :: Num a => V4 a
bgColor = V4 63 63 63 255

playerColor :: Num a => V4 a
playerColor = V4 255 255 255 255

terrainColor :: Num a => V4 a
terrainColor = V4 0 255 0 255

treeColor :: Num a => V4 a
treeColor = V4 255 127 0 255
