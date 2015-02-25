import Control.Concurrent.STM
import Graphics.UI.GLUT hiding (Level)
import Graphics.UI.GLUT.Menu
import Control.Applicative

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
    game   <- newTVarIO $ Game ([Void, Ib, Ib, Ib, Ib]:(replicate 17 [])) (Level 10 500) [] 0
    windowSize $= Size windowHeight windowWidth
    viewport $= (Position 0 0, Size windowWidth windowHeight)
    displayCallback $= displayGame game
    reshapeCallback $= Just reshape
    keyboardMouseCallback $= Just (keyboardHandler queue paused)

    addTimerCallback 500 (dropOne game)
    --idleCallback $= Just (dropOne game)

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

dropOne :: TVar Game -> TimerCallback
dropOne tGame = do
    f <- atomically $ do
        modifyTVar tGame rot
        (frequency.level) <$> (readTVar tGame)
    postRedisplay Nothing
    addTimerCallback f (dropOne tGame)
    where rot :: Game -> Game
          rot g = case world g of
                    [] -> g {world = []}
                    (h:t) -> g {world = (t ++ [h])}
