module Main where

import Control.Monad
import Graphics.UI.Gtk
import Control.Concurrent
import Graphics.Rendering.Cairo
import Sokoban
import System.Directory (getDirectoryContents, setCurrentDirectory)
import Data.List
import System.FilePath

--TODO: Handle Keyboard input. Draw on window

type Tile = (String, IO Surface)
data Tiles = Tiles {bg :: Surface, pl :: Surface, wall :: Surface, crate :: Surface, target :: Surface}

drawLevel canvas tiles = liftIO $ do
        renderWithDrawable canvas $ do
            setSourceSurface (bg tiles) 100 100
            paint
        return True

--loadTiles :: IO [Tile]
loadTiles = do
        bgImg <- imageSurfaceCreateFromPNG "tiles/freeBackground.png"
        wallImg <- imageSurfaceCreateFromPNG "tiles/wall.png"
        playerImg <- imageSurfaceCreateFromPNG "tiles/wall.png"
        crateImg <- imageSurfaceCreateFromPNG "tiles/wall.png"
        targetImg <- imageSurfaceCreateFromPNG "tiles/wall.png"

        return Tiles{bg = bgImg, pl = playerImg, wall = wallImg, crate = crateImg, target = targetImg}
        
main :: IO ()
main = do
    initGUI
    window <- windowNew
    window `on` sizeRequest     $ return (Requisition 800 600)
    
    frame <- frameNew
    containerAdd window frame
    canvas <- drawingAreaNew
    containerAdd frame canvas
    

    onDestroy window mainQuit
    widgetShowAll window

    drawin <- widgetGetDrawWindow canvas
    tiles <- loadTiles
    canvas `on` exposeEvent $ drawLevel drawin tiles

    mainGUI
