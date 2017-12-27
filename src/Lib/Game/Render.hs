{-# LANGUAGE QuasiQuotes #-}

module Lib.Game.Render (renderWorld) where

import Lib.Game.Dropdown (Dropdown)
import Lib.Game.Object (Object)
import Lib.Graphics.RenderContext (RenderContext(..))
import Lib.Graphics.Scene (Scene)
import Lib.Model.Spaces
import Lib.Game.World
import Lib.Util

import qualified Lib.Game.Dropdown as Dropdown
import qualified Lib.Game.Object as Object
import qualified Lib.Game.UserCommand as UserCommand
import qualified Lib.Graphics.Camera as Camera
import qualified Lib.Graphics.Scene as Scene

import Control.Lens
import Data.Generics.Product (field)
import Data.Monoid
import Data.String.Interpolate.IsString (i)
import Linear
import SDL (($=))

import qualified SDL as Sdl



renderWorld :: RenderContext -> World -> IO ()
renderWorld renderCtx@RenderContext{ renderer } world = do
  Sdl.rendererDrawColor renderer $= bgColor
  Sdl.clear renderer
  let scene = case world ^. field @"mapView" of
        Global -> globalScene world
        Local -> localScene world
  Scene.render renderCtx scene
  Sdl.present renderer



globalScene :: World -> Scene (Screen Double)
globalScene world =
  Scene.vmap (Camera.project cam)
  $ globalTiles visibleTiles world
  where
    visibleTiles = rangeZip (topLeft, bottomRight)
    topLeft = floor <$> Camera.invProject cam 0
    bottomRight = floor <$> Camera.invProject cam screenSize
    cam = globalCamera world

localScene :: World -> Scene (Screen Double)
localScene world =
  Scene.vmap (Camera.project cam)
    (localTiles visibleTiles world)
  <> inventoryScene world
  <> foldMap (dropdownScene world) (world ^. field @"activeDropdown")
  where
    visibleTiles = rangeZip (topLeft, bottomRight)
    topLeft = floor <$> Camera.invProject cam 0
    bottomRight = floor <$> Camera.invProject cam screenSize
    cam = localCamera world



globalTiles :: [ChunkV Int] -> World -> Scene (Chunk Double)
globalTiles (filter validChunk -> visibleChunks) world =
  globalTerrainTiles <> (Chunk . unTile <$> playerTile)
  where
    globalTerrainTiles = foldMap (globalTerrainTile world) visibleChunks
    playerTile = Scene.tileCenteredRectangle
      (world ^. field @"playerChunk") playerSize playerColor

globalTerrainTile :: World -> ChunkV Int -> Scene (Chunk Double)
globalTerrainTile world chunk = terrain <> villageMarker <> loadMarker
  where
    terrain = Scene.tileCenteredRectangle chunk 1 (Scene.Solid color)
    color = floor <$> lerp gradient forestColor plainsColor
    gradient = global ^. field @"treeDensity"
    villageMarker
      | global ^. field @"hasVillage" =
          Scene.tileCenteredRectangle chunk villageMarkerSize villageMarkerColor
      | otherwise = mempty
    global = world ^. field @"chunkGlobals" . singular (ix chunk)
    loadMarker = case world ^. field @"loadedChunkLocals" . at chunk of
      Just _ -> Scene.tileCenteredRectangle chunk 0.1 (Scene.Outline 255)
      Nothing -> mempty



localTiles :: [InChunkV Int] -> World -> Scene (InChunk Double)
localTiles visibleTiles world = localTerrainObjTiles <> playerTile
  where
    localTerrainObjTiles = foldMap (localTerrainObjTile world) visibleTiles
    playerTile = Scene.tileCenteredRectangle
      (world ^. field @"playerPos") playerSize playerColor

localTerrainObjTile :: World -> InChunkV Int -> Scene (InChunk Double)
localTerrainObjTile world tile =
  case localAtChunk of
    Just local ->
      let objTiles =
            foldMap (localObjTile tile)
            . view (field @"objects" . at normTile . _Just)
            $ local
      in terrainTile <> objTiles
    Nothing -> mempty
  where
    localAtChunk = world ^. field @"loadedChunkLocals" . at chunk
    (chunk, normTile) = normalizeChunkPos (world ^. field @"playerChunk") tile
    terrainTile = Scene.tileCenteredRectangle tile 1 terrainColor

localObjTile :: InChunkV Int -> Object -> Scene (InChunk Double)
localObjTile tile object = Scene.tileCenteredRectangle tile size color
  where
    (size, color) = case object of
      Object.Tree -> (treeSize, treeColor)
      Object.Arrow -> (arrowSize, arrowColor)
      Object.Deer -> (deerSize, deerColor)
      Object.Meat -> (meatSize, meatColor)
      Object.Wall -> (1, Scene.Solid $ V3 190 100 20)
      Object.Villager -> (inChunkV 0.3 0.8, Scene.Solid $ V3 255 255 31)
      Object.Gold -> (0.2, Scene.Solid $ V3 255 255 0)



inventoryScene :: World -> Scene (Screen Double)
inventoryScene world =
  Scene.text 0 "Inventory" 255
  <> (ifoldMap line . map lineStr . itoList . view (field @"inventory")) world
  where
    line row str =
      let pos = screenV 0 (fromIntegral $ (row + 1) * 16)
      in Scene.text pos str 255
    lineStr (obj, count) = [i|#{count} #{Lower obj}|]

dropdownScene :: World -> Dropdown (InChunkV Double) -> Scene (Screen Double)
dropdownScene world dropdown =
  Dropdown.render showCommand
  . fmap (Camera.project $ localCamera world)
  $ dropdown
  where
    showCommand = \case
      UserCommand.ShootArrow _ -> "Shoot arrow"
      UserCommand.GetObject _ _ o -> [i|Get #{Lower o}|]
      UserCommand.TradeObjects givenQty givenObj recvdQty recvdObj ->
        [i|Trade #{givenQty} #{Lower givenObj} for #{recvdQty} #{Lower recvdObj}|]



playerSize :: Fractional a => V2 a
playerSize = 0.7

treeSize :: InChunkV Double
treeSize = 0.75

arrowSize :: InChunkV Double
arrowSize = inChunkV 0.9 0.1

deerSize :: InChunkV Double
deerSize = inChunkV 0.9 0.6

meatSize :: InChunkV Double
meatSize = inChunkV 0.8 0.4

villageMarkerSize :: ChunkV Double
villageMarkerSize = 0.8

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

villageMarkerColor :: Scene.Style
villageMarkerColor = treeColor
