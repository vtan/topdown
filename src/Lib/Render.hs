module Lib.Render (renderWorld) where

import Lib.ChunkData
import Lib.Spaces
import Lib.World

import Control.Lens
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
    ChunkV (V2 left top) = chn 0
    ChunkV (V2 right bottom) = chn screenSize
    tiles = filter validChunk [chunkV x y
      | x <- [left .. right]
      , y <- [top .. bottom]
      ]
  for_ tiles $ \i -> do
    let global = world ^. chunkGlobals . arrayAt i
        color = floor <$> lerp (global ^. treeDensity) forestColor plainsColor
        scri = scr . TileV . unChunkV $ i
    rendererDrawColor renderer $= color
    fillRect renderer . Just $ tileRectangle tileSize scri
    when (has (loadedChunkLocals . at i . _Just) world) $ do
      rendererDrawColor renderer $= 255
      drawRect renderer . Just $ tileRectangle 3 scri

  rendererDrawColor renderer $= playerColor
  fillRect renderer . Just
    $ tileRectangle playerSize (world ^. playerChunk . tiledChunk . to scr)
  where
    scr = tilesToScr tileSize (world ^. playerChunk . tiledChunk) playerEyeOnScr
    chn = view (from tiledChunk) . scrToTiles tileSize playerEyeOnScr (world ^. playerChunk . tiledChunk)

renderLocal :: Renderer -> World -> IO ()
renderLocal renderer world = do
  let
    InChunkV (V2 left top) = chn 0
    InChunkV (V2 right bottom) = chn screenSize
    tiles = [inChunkV x y
      | x <- [left .. right]
      , y <- [top .. bottom]
      ]
  for_ tiles $ \pos -> do
    let (i, pos') = normalizeChunkPos (world ^. playerChunk) pos
    case world ^. loadedChunkLocals . at i of
      Just chunkLocal -> do
        rendererDrawColor renderer $= terrainColor
        fillRect renderer . Just $ tileRectangle tileSize (scr . view tiledInChunk $ pos)
        for_ (chunkLocal ^. objects . at pos' . traversed) $
          renderObject renderer (scr . view tiledInChunk $ pos)
      Nothing -> pure ()

  rendererDrawColor renderer $= playerColor
  fillRect renderer . Just
    $ tileRectangle playerSize (world ^. playerPos . tiledInChunk . to scr)
  where
    scr = tilesToScr tileSize (world ^. playerPos . tiledInChunk) playerEyeOnScr
    chn = view (from tiledInChunk) . scrToTiles tileSize playerEyeOnScr (world ^. playerPos . tiledInChunk)

renderObject :: Integral a => Renderer -> ScreenV a -> Object -> IO ()
renderObject renderer pos object = do
  rendererDrawColor renderer $= color
  fillRect renderer . Just $ tileRectangle size pos
  where
    (color, size) = case object of
      Tree -> (treeColor, treeSize)
      Arrow -> (arrowColor, arrowSize)

tileRectangle :: (Integral a, Num b) => ScreenV a -> ScreenV a -> Rectangle b
tileRectangle size tileTopLeft = fromIntegral <$> Rectangle rectTopLeft bounds
  where
    rectTopLeft = P . unScreenV $ tileTopLeft + ((`quot` 2) <$> tileSize - size)
    bounds = unScreenV size



playerSize :: Num a => ScreenV a
playerSize = screenV 20 20

treeSize :: Num a => ScreenV a
treeSize = screenV 28 28

arrowSize :: Num a => ScreenV a
arrowSize = screenV 30 4

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
