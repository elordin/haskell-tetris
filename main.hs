import Control.Concurrent.STM
import Graphics.UI.GLUT hiding (Level)
import Graphics.UI.GLUT.Menu
import Control.Applicative
import System.Exit

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
    game   <- newTVarIO $ GameMenu Quit
    --game   <- newTVarIO $ Game ([Void, Ib, Ib, Ib, Ib]:(replicate 17 [])) (Level 10 500) [] 0
    windowSize $= Size windowHeight windowWidth
    viewport $= (Position 0 0, Size windowWidth windowHeight)
    displayCallback $= displayGame game
    reshapeCallback $= Just reshape
    keyboardMouseCallback $= Just (keyboardHandler queue paused)

    --addTimerCallback 500 (dropOne game)
    idleCallback $= Just (menuHandler queue game)

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


menuHandler :: TVar [Key] -> TVar Game -> IdleCallback
menuHandler queue game = do
    input <- atomically $ do
        q <- readTVar queue
        case q of
            []    -> return Nothing
            (h:t) -> do
                writeTVar queue t
                return $ Just h
    case input of
        Just (SpecialKey KeyUp)   -> do { atomically $ modifyTVar game toggleMenuItem; postRedisplay Nothing }
        Just (SpecialKey KeyDown) -> do { atomically $ modifyTVar game toggleMenuItem; postRedisplay Nothing }
        Just (Char '\n')          -> do { select; postRedisplay Nothing }
        Just (Char '\r')          -> do { select; postRedisplay Nothing }
        _                         -> return ()
    where toggleMenuItem (GameMenu Start) = GameMenu Quit
          toggleMenuItem (GameMenu Quit)  = GameMenu Start
          toggleMenuItem g = g
          select = do
            g <- atomically $ readTVar game
            case g of
                GameMenu Quit          -> exitSuccess
                GameMenu Start         -> atomically $ writeTVar game $ Game ([Void, Ib, Ib, Ib, Ib]:(replicate 17 [])) (Level 10 500) [] 0 False
                g@(Game _ _ _ _ True)  -> atomically $ writeTVar game $ g { paused = False}
                g@(Game _ _ _ _ False) -> atomically $ writeTVar game $ g { paused = True }

--dropOne :: TVar Game -> TimerCallback
--dropOne tGame = do
--    f <- atomically $ do
--        modifyTVar tGame rot
--        (frequency.level) <$> (readTVar tGame)
--    postRedisplay Nothing
--    addTimerCallback f (dropOne tGame)
--    where rot :: Game -> Game
--          rot g = case world g of
--                    [] -> g {world = []}
--                    (h:t) -> g {world = (t ++ [h])}

