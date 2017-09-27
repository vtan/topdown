module Lib.Render (renderWorld) where

import Lib.ChunkData
import Lib.Scene (Scene)
import Lib.Spaces
import Lib.Util
import Lib.World

import qualified Lib.Scene as Scene

import Control.Lens
import Data.Char
import Data.Monoid
import SDL

import qualified SDL.Font as Sdl.Font
import qualified Data.Text as Text



renderWorld :: Renderer -> Sdl.Font.Font -> World -> IO ()
renderWorld renderer font world = do
  rendererDrawColor renderer $= bgColor
  clear renderer
  let scene = case world ^. mapView of
        Global -> globalScene world
        Local -> localScene world
  Scene.render renderer font scene
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
  Scene.vmap (localTileToScreen world) (localTiles visibleTiles world)
  <> inventoryScene world
  <> foldMap (dropdownScene world) (world ^. activeDropdown)
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
      Deer -> (deerSize, deerColor)
      Meat -> (meatSize, meatColor)



inventoryScene :: World -> Scene ScreenV Double
inventoryScene world =
  Scene.text 0 "Inventory" 255
  <> (ifoldMap line . map lineStr . itoList . view inventory) world
  where
    line i str =
      let pos = screenV 0 (fromIntegral $ (i + 1) * 16)
      in Scene.text pos str 255
    lineStr (obj, count) = Text.pack $ unwords [show count, show obj]

dropdownScene :: World -> Dropdown -> Scene ScreenV Double
dropdownScene world (Dropdown anchor items) =
  fromIntegral <$> ifoldMap item items
  where
    item i (DropdownItem cmd _) =
      let pos = anchorScr + screenV 0 (i * dropdownItemSize ^. _y)
          str = case cmd of
            ShootArrow -> "Shoot arrow"
            GetObject o -> Text.pack $ unwords ["Get", over _head toLower $ show o]
      in Scene.rectangle pos (pos + dropdownItemSize) dropdownItemColor
         <> Scene.text pos str 255
    anchorScr = floor <$> localTileToScreen world anchor



playerSize :: IsTileV t => t Double
playerSize = view (from _V2) 0.7

treeSize :: IsTileV t => t Double
treeSize = view (from _V2) 0.75

arrowSize :: IsTileV t => t Double
arrowSize = view (from _V2) $ V2 0.9 0.1

deerSize :: IsTileV t => t Double
deerSize = view (from _V2) $ V2 0.9 0.6

meatSize :: IsTileV t => t Double
meatSize = view (from _V2) $ V2 0.8 0.4

dropdownItemColor :: Scene.Style
dropdownItemColor = Scene.Solid 127

bgColor :: Num a => V4 a
bgColor = V4 63 63 63 255

playerColor :: Scene.Style
playerColor = Scene.Solid 255

terrainColor :: Scene.Style
terrainColor = Scene.Solid $ V3 0 201 0

treeColor :: Scene.Style
treeColor = Scene.Solid $ V3 157 93 17

arrowColor :: Scene.Style
arrowColor = Scene.Outline 255

deerColor :: Scene.Style
deerColor = Scene.Solid $ V3 210 93 17

meatColor :: Scene.Style
meatColor = Scene.Solid $ V3 210 17 180

plainsColor :: Num a => V3 a
plainsColor = V3 0 255 0

forestColor :: Num a => V3 a
forestColor = V3 0 31 0
