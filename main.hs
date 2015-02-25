import Control.Concurrent.STM
import Graphics.UI.GLUT
import Graphics.UI.GLUT.Menu

import Util
import Game
import KeyboardHandler
import Display

windowWidth  = 720
windowHeight = 720

main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize
    window <- createWindow "Tretis"
    queue  <- newTVarIO []
    paused <- newTVarIO True
    game   <- newTVarIO $ Game $ World $ [[Tb, Void, Void, Void, Tb, Tb, Tb], [Tb, Tb, Void, Void, Lb, Tb, Void], [Tb, Void, Void, Void, Lb, Lb, Lb]]
    print $ World $ [[Tb, Void, Void, Void, Tb, Tb, Tb], [Tb, Tb, Void, Void, Lb, Tb, Void], [Tb, Void, Void, Void, Lb, Lb, Lb]]
    windowSize $= Size windowHeight windowWidth
    viewport $= (Position 0 0, Size windowWidth windowHeight)
    displayCallback $= displayGame game
    reshapeCallback $= Just reshape
    keyboardMouseCallback $= Just (keyboardHandler queue paused)
    fullScreen
    mainLoop

reshape :: ReshapeCallback
reshape size = do
    let Size w h = size
        newsize :: GLsizei
        newsize  = min w h
        offsetY :: GLint
        offsetY  = (h - newsize)
    viewport    $= (Position 0 offsetY, Size newsize newsize)
    postRedisplay Nothing
