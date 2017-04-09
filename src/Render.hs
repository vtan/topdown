module Render where

import Spaces
import World

import Data.Foldable (for_)
import SDL



renderWorld :: Renderer -> World -> IO ()
renderWorld renderer world = do
  rendererDrawColor renderer $= bgColor
  clear renderer

  rendererDrawColor renderer $= terrainColor
  for_ chunkRelPositions $ \pos -> do
    fillRect renderer . Just $ rect pos

  rendererDrawColor renderer $= treeColor
  for_ (treeRelPositions $ chunk world) $ \pos -> do
    fillRect renderer . Just $ rect pos

  rendererDrawColor renderer $= playerColor
  fillRect renderer . Just $ rect (playerPos world)

  present renderer
  where
    rect p = fromIntegral
      <$> Rectangle (P . unScr2 $ scr p - halfTile) (unScr2 tileSize)
    scr = chnToScr tileSize (playerPos world) midScr
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
