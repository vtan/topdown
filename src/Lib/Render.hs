module Lib.Render (renderWorld) where

import Lib.ChunkData
import Lib.Scene (Scene)
import Lib.Spaces
import Lib.Util
import Lib.World

import qualified Lib.Scene as Scene

import Control.Lens
import Data.Monoid
import SDL



renderWorld :: Renderer -> World -> IO ()
renderWorld renderer world = do
  rendererDrawColor renderer $= bgColor
  clear renderer
  let scene = case world ^. mapView of
        Global -> globalScene world
        Local -> localScene world
  Scene.render renderer scene
  present renderer



globalScene :: World -> Scene ScreenV Double
globalScene world =
  Scene.vmap (globalTileToScreen world)
  $ globalTiles visibleTiles world
  where
    visibleTiles = rangeZip (topLeft, bottomRight)
    topLeft = screenToGlobalTile world 0
    bottomRight = screenToGlobalTile world screenSize

localScene :: World -> Scene ScreenV Double
localScene world =
  Scene.vmap (localTileToScreen world)
  $ localTiles visibleTiles world
  where
    visibleTiles = rangeZip (topLeft, bottomRight)
    topLeft = screenToLocalTile world 0
    bottomRight = screenToLocalTile world screenSize



globalTiles :: [ChunkV Int] -> World -> Scene ChunkV Double
globalTiles (filter validChunk -> visibleChunks) world =
  globalTerrainTiles <> playerTile
  where
    globalTerrainTiles = foldMap (globalTerrainTile world) visibleChunks
    playerTile = Scene.tileCenteredRectangle
      (world ^. playerChunk) playerSize playerColor

globalTerrainTile :: World -> ChunkV Int -> Scene ChunkV Double
globalTerrainTile world chunk = terrain <> loadMarker
  where
    terrain = Scene.tileCenteredRectangle chunk 1 (Scene.Solid color)
    color = floor <$> lerp gradient forestColor plainsColor
    gradient = world ^. chunkGlobals . arrayAt chunk . treeDensity
    loadMarker = case world ^. loadedChunkLocals . at chunk of
      Just _ -> Scene.tileCenteredRectangle chunk 0.1 (Scene.Outline 255)
      Nothing -> mempty



localTiles :: [InChunkV Int] -> World -> Scene InChunkV Double
localTiles visibleTiles world = localTerrainObjTiles <> playerTile
  where
    localTerrainObjTiles = foldMap (localTerrainObjTile world) visibleTiles
    playerTile = Scene.tileCenteredRectangle
      (world ^. playerPos) playerSize playerColor

localTerrainObjTile :: World -> InChunkV Int -> Scene InChunkV Double
localTerrainObjTile world tile =
  case localAtChunk of
    Just local ->
      let objTiles =
            foldMap (localObjTile tile)
            . view (objects . at normTile . _Just)
            $ local
      in terrainTile <> objTiles
    Nothing -> mempty
  where
    localAtChunk = world ^. loadedChunkLocals . at chunk
    (chunk, normTile) = normalizeChunkPos (world ^. playerChunk) tile
    terrainTile = Scene.tileCenteredRectangle tile 1 terrainColor

localObjTile :: InChunkV Int -> Object -> Scene InChunkV Double
localObjTile tile object = Scene.tileCenteredRectangle tile size color
  where
    (size, color) = case object of
      Tree -> (treeSize, treeColor)
      Arrow -> (arrowSize, arrowColor)



globalTileToScreen :: Num a => World -> ChunkV a -> ScreenV a
globalTileToScreen world =
  tilesToScr
    tileSize
    (fromIntegral <$> world ^. playerChunk)
    (fromIntegral <$> playerEyeOnScr)

screenToGlobalTile :: World -> ScreenV Int -> ChunkV Int
screenToGlobalTile world =
  scrToTiles tileSize playerEyeOnScr (world ^. playerChunk)

localTileToScreen :: Num a => World -> InChunkV a -> ScreenV a
localTileToScreen world =
  tilesToScr
    tileSize
    (fromIntegral <$> world ^. playerPos)
    (fromIntegral <$> playerEyeOnScr)

screenToLocalTile :: World -> ScreenV Int -> InChunkV Int
screenToLocalTile world =
  scrToTiles tileSize playerEyeOnScr (world ^. playerPos)



playerSize :: IsTileV t => t Double
playerSize = view (from _V2) 0.7

treeSize :: IsTileV t => t Double
treeSize = view (from _V2) 0.75

arrowSize :: IsTileV t => t Double
arrowSize = view (from _V2) $ V2 0.9 0.1

bgColor :: Num a => V4 a
bgColor = V4 63 63 63 255

playerColor :: Scene.Color
playerColor = Scene.Solid 255

terrainColor :: Scene.Color
terrainColor = Scene.Solid $ V3 0 201 0

treeColor :: Scene.Color
treeColor = Scene.Solid $ V3 157 93 17

arrowColor :: Scene.Color
arrowColor = Scene.Outline 255

plainsColor :: Num a => V3 a
plainsColor = V3 0 255 0

forestColor :: Num a => V3 a
forestColor = V3 0 31 0
