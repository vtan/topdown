module Render where

import Chunk
import Spaces
import World

import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Foldable (for_)
import Linear
import SDL



renderWorld :: Renderer -> World -> IO World
renderWorld renderer world = do
  rendererDrawColor renderer $= bgColor
  clear renderer
  world' <- case world ^. mapView of
    Global -> renderGlobal renderer world
    Local -> renderLocal renderer world
  present renderer
  pure world'



renderGlobal :: Renderer -> World -> IO World
renderGlobal renderer world = do
  let
    ChnIdx (V2 left top) = chn 0
    ChnIdx (V2 right bottom) = chn screenSize
    tiles = [ChnIdx $ V2 x y
      | x <- [left .. right]
      , y <- [top .. bottom]
      ]
  world' <- flip execStateT world $
    for_ tiles $ \i -> do
      (chunkGlobal, _) <- getChunkAt i
      rendererDrawColor renderer
        $= floor <$> lerp (chunkGlobal ^. treeDensity) forestColor plainsColor
      fillRect renderer . Just $ rectangleAt tileSize (scr i)

  rendererDrawColor renderer $= playerColor
  fillRect renderer . Just
    $ rectangleAt playerSize (world ^. playerChunk . to scr)

  pure world'
  where
    scr = tilesToScr tileSize (world ^. playerChunk) midScr
    chn = scrToTiles tileSize midScr (world ^. playerChunk)
    midScr = (`quot` 2) <$> screenSize

renderLocal :: Renderer -> World -> IO World
renderLocal renderer world = do
  let
    Chn2 (V2 left top) = chn 0
    Chn2 (V2 right bottom) = chn screenSize
    tiles = [Chn2 $ V2 x y
      | x <- [left .. right]
      , y <- [top .. bottom]
      ]
  world' <- flip execStateT world $
    for_ tiles $ \pos -> do
      let (i, pos') = normalizeChunkPos (world ^. playerChunk) pos
      (_, chunkLocal) <- getChunkAt i
      rendererDrawColor renderer $= terrainColor
      fillRect renderer . Just $ rectangleAt tileSize (scr pos)
      when (chunkLocal ^. treeRelPositions . contains pos') $ do
        rendererDrawColor renderer $= treeColor
        fillRect renderer . Just $ rectangleAt treeSize (scr pos)

  rendererDrawColor renderer $= playerColor
  fillRect renderer . Just
    $ rectangleAt playerSize (world ^. playerPos . to scr)

  pure world'
  where
    scr = tilesToScr tileSize (world ^. playerPos) midScr
    chn = scrToTiles tileSize midScr (world ^. playerPos)
    midScr = (`quot` 2) <$> screenSize

rectangleAt :: (Integral a, Num b) => Scr2 a -> Scr2 a -> Rectangle b
rectangleAt size mid = fromIntegral <$> Rectangle topLeft bounds
  where
    topLeft = P $ unScr2 mid - ((`quot` 2) <$> bounds)
    bounds = unScr2 size



screenSize :: Num a => Scr2 a
screenSize = Scr2 $ V2 800 600

tileSize :: Num a => Scr2 a
tileSize = Scr2 $ V2 32 32

playerSize :: Num a => Scr2 a
playerSize = Scr2 $ V2 20 20

treeSize :: Num a => Scr2 a
treeSize = Scr2 $ V2 28 28

bgColor :: Num a => V4 a
bgColor = V4 63 63 63 255

playerColor :: Num a => V4 a
playerColor = V4 255 255 255 255

terrainColor :: Num a => V4 a
terrainColor = V4 0 201 0 255

treeColor :: Num a => V4 a
treeColor = V4 157 93 17 255

plainsColor :: Num a => V4 a
plainsColor = V4 0 255 0 255

forestColor :: Num a => V4 a
forestColor = V4 0 31 0 255
