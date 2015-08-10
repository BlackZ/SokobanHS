module Main where

import Control.Monad
import Graphics.UI.Gtk
import Control.Concurrent
import Control.Concurrent.MVar
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.Events
import Sokoban
import System.Directory (getDirectoryContents, setCurrentDirectory)
import Data.List
import System.FilePath
import Prelude hiding(Either(..))



type Tile = (String, IO Surface)
data Tiles = Tiles {bg :: Surface, pl :: Surface, wall :: Surface, crate :: Surface, target :: Surface}

data State = State {levels :: [Level], curLevelCounter :: Int, curLevel :: Level}

tileWidth = 100
tileHeight = 85



drawTile :: Surface -> Coord -> Render ()
drawTile tile (x,y) =  do
                        setSourceSurface tile (fromIntegral (x*tileWidth)) (fromIntegral (y*tileHeight))
                        paint

                    
drawLevel :: Tiles -> MVar State -> Render ()
drawLevel tiles stateMV = do
                        state <- liftIO $ readMVar stateMV
                        let level = curLevel state                        
                        mapM (drawTile (wall tiles)) (walls level)
                        mapM (drawTile (target tiles)) (targets level)
                        mapM (drawTile (crate tiles)) (crates level)
                        drawTile (pl tiles) (player level)
                        setFontSize 28.0
                        setSourceRGBA 1 0 0 1.0
                        setLineWidth 3
                        moveTo 20 50
                        let text = "Steps: " ++ show (steps level)
                        showText text
                        when (isSolved level) $ drawText "Level done!" (maxSize level)

drawText :: String -> Coord -> Render ()
drawText s (x,y) = do
                stringSize <- textExtents s
                let stringW = textExtentsWidth stringSize
                moveTo (fromIntegral (x*tileWidth) / 2.0 - stringW / 2) (fromIntegral (y*tileHeight) / 2.0)
                setFontSize 50
                setSourceRGBA 1 0 0 1.0
                setLineWidth 3
                showText s
                        
loadTiles :: IO Tiles
loadTiles = do
        bgImg <- imageSurfaceCreateFromPNG "tiles/freeBackground.png"
        wallImg <- imageSurfaceCreateFromPNG "tiles/wall.png"
        playerImg <- imageSurfaceCreateFromPNG "tiles/player.png"
        crateImg <- imageSurfaceCreateFromPNG "tiles/crate.png"
        targetImg <- imageSurfaceCreateFromPNG "tiles/target.png"

        return Tiles{bg = bgImg, pl = playerImg, wall = wallImg, crate = crateImg, target = targetImg}

parseLevel :: IO State
parseLevel = do
                setCurrentDirectory "level"
                allFiles <- getDirectoryContents "."
                let fileNames = sort (filter (isSuffixOf ".lvl") allFiles)
                files <- mapM (readFile) fileNames
                setCurrentDirectory ".."
                lvls <- return $ map loadLevel files
                if null lvls
                    then do
                            putStrLn "No levels could be read. Check the level files in the 'level' folder"
                            mainQuit
                            return State{levels=[emptyLevel], curLevelCounter=0, curLevel=emptyLevel}
                    else return State {levels = lvls, curLevelCounter=0, curLevel = lvls!!0}


handleKeyboard :: MVar State -> Window -> Event -> IO ()
handleKeyboard stateMV window key = do
                                    state <- liftIO $ takeMVar stateMV
                                    let lvl = curLevel state
                                    let keyChar = eventKeyChar key
                                    putStrLn ("Handle key " ++ show(keyChar))
                                    case keyChar of
                                        Just 'w' -> performAction state Up 
                                        Just 's' -> performAction state Down
                                        Just 'a' -> performAction state Left
                                        Just 'd' -> performAction state Right
                                        Just 'r' -> performUndo state
                                        Just 'q' -> do
                                                        widgetDestroy window
                                                        return ()
                                        Just 'n' -> loadNextLevel state
                                        Just 'p' -> loadPrevLevel state
--                                        Just 'c' -> do
--                                                    putStrLn (show(solveLevel lvl))
--                                                    putMVar stateMV state
                                        otherwise -> do
                                                    putStrLn ("Unknown key " ++ show(keyChar))
                                                    putMVar stateMV state
                                    widgetQueueDraw window

                                    
                                    where 
                                        performAction state cmd = do
                                                        let updatedLevel = step (curLevel state) cmd
                                                        if isSolved (curLevel state)
                                                            then loadNextLevel state
                                                            else do
                                                                    when (isSolved updatedLevel) $ putStrLn "Level done!"
                                                                    putMVar stateMV $ state{curLevel = updatedLevel}
                                        performUndo state = do
                                                        putMVar stateMV $ state{curLevel = stepBack (curLevel state)}

                                        loadNextLevel state@State {curLevelCounter = counter, levels = lvls}
                                            | counter + 1 < length lvls = putMVar stateMV $ state{curLevel = lvls!!(counter+1), curLevelCounter = counter+1}
                                            | otherwise = do
                                                             putStrLn "No more level to load." 
                                                             putMVar stateMV $ state 
                                                             return ()
                                        loadPrevLevel state@State {curLevelCounter = counter, levels = lvls}
                                            | counter -1 >= 0 = putMVar stateMV $ state{curLevel = lvls!!(counter-1), curLevelCounter = counter-1}
                                            | otherwise = putMVar stateMV $ state


resizeWindow :: Window -> MVar State -> IO ()
resizeWindow w mv = do
                      state <- liftIO $ readMVar mv
                      let level = curLevel state  
                      let (width, height) = maxSize level
                      windowResize w ((width+1)*tileWidth) ((height+1)*tileHeight)    

main :: IO ()
main = do
    initGUI
    window <- windowNew
    set window [windowTitle := "Sokoban", windowAllowGrow := True, windowDefaultWidth := 400, windowDefaultHeight := 600]
    
    frame <- frameNew
    containerAdd window frame
    

    onDestroy window mainQuit
    widgetShowAll window

    drawin <- widgetGetDrawWindow window
    tiles <- loadTiles
    state <- parseLevel
    stateMV <- newMVar state 
    onExpose window (\x -> do resizeWindow window stateMV
                              renderWithDrawable drawin (drawLevel tiles stateMV) 
                                
                              return True)
    onKeyPress window (\x -> do handleKeyboard stateMV window x
                                return True)
    
    mainGUI
