module Render (renderWorld) where

import ChunkData
import Spaces
import World

import Control.Lens (_Just, at, contains, has, to)
import Control.Lens.Operators
import Control.Monad (when)
import Data.Foldable (for_)
import SDL



renderWorld :: Renderer -> World -> IO ()
renderWorld renderer world = do
  rendererDrawColor renderer $= bgColor
  clear renderer
  case world ^. mapView of
    Global -> renderGlobal renderer world
    Local -> renderLocal renderer world
  present renderer



renderGlobal :: Renderer -> World -> IO ()
renderGlobal renderer world = do
  let
    ChnIdx (V2 left top) = chn 0
    ChnIdx (V2 right bottom) = chn screenSize
    tiles = filter validChunk [ChnIdx $ V2 x y
      | x <- [left .. right]
      , y <- [top .. bottom]
      ]
  for_ tiles $ \i -> do
    let global = world ^. chunkGlobals . arrayAt i
        color = floor <$> lerp (global ^. treeDensity) forestColor plainsColor
        scri = scr i
    rendererDrawColor renderer $= color
    fillRect renderer . Just $ tileRectangle tileSize scri
    when (has (loadedChunkLocals . at i . _Just) world) $ do
      rendererDrawColor renderer $= 255
      drawRect renderer . Just $ tileRectangle 3 scri

  rendererDrawColor renderer $= playerColor
  fillRect renderer . Just
    $ tileRectangle playerSize (world ^. playerChunk . to scr)
  where
    scr = tilesToScr tileSize (world ^. playerChunk) playerEyeOnScr
    chn = scrToTiles tileSize playerEyeOnScr (world ^. playerChunk)

renderLocal :: Renderer -> World -> IO ()
renderLocal renderer world = do
  let
    Chn2 (V2 left top) = chn 0
    Chn2 (V2 right bottom) = chn screenSize
    tiles = [Chn2 $ V2 x y
      | x <- [left .. right]
      , y <- [top .. bottom]
      ]
  for_ tiles $ \pos -> do
    let (i, pos') = normalizeChunkPos (world ^. playerChunk) pos
    case world ^. loadedChunkLocals . at i of
      Just chunkLocal -> do
        rendererDrawColor renderer $= terrainColor
        fillRect renderer . Just $ tileRectangle tileSize (scr pos)
        when (chunkLocal ^. trees . contains pos') $ do
          rendererDrawColor renderer $= treeColor
          fillRect renderer . Just $ tileRectangle treeSize (scr pos)
        when (chunkLocal ^. arrows . contains pos') $ do
          rendererDrawColor renderer $= arrowColor
          fillRect renderer . Just $ tileRectangle arrowSize (scr pos)
      Nothing -> pure ()

  rendererDrawColor renderer $= playerColor
  fillRect renderer . Just
    $ tileRectangle playerSize (world ^. playerPos . to scr)
  where
    scr = tilesToScr tileSize (world ^. playerPos) playerEyeOnScr
    chn = scrToTiles tileSize playerEyeOnScr (world ^. playerPos)

tileRectangle :: (Integral a, Num b) => Scr2 a -> Scr2 a -> Rectangle b
tileRectangle size tileTopLeft = fromIntegral <$> Rectangle rectTopLeft bounds
  where
    rectTopLeft = P . unScr2 $ tileTopLeft + ((`quot` 2) <$> tileSize - size)
    bounds = unScr2 size



playerSize :: Num a => Scr2 a
playerSize = Scr2 $ V2 20 20

treeSize :: Num a => Scr2 a
treeSize = Scr2 $ V2 28 28

arrowSize :: Num a => Scr2 a
arrowSize = Scr2 $ V2 30 4

bgColor :: Num a => V4 a
bgColor = V4 63 63 63 255

playerColor :: Num a => V4 a
playerColor = V4 255 255 255 255

terrainColor :: Num a => V4 a
terrainColor = V4 0 201 0 255

treeColor :: Num a => V4 a
treeColor = V4 157 93 17 255

arrowColor :: Num a => V4 a
arrowColor = V4 255 255 255 255

plainsColor :: Num a => V4 a
plainsColor = V4 0 255 0 255

forestColor :: Num a => V4 a
forestColor = V4 0 31 0 255
