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
    game   <- newTVarIO $ GameMenu Start
    --game   <- newTVarIO $ Game ([Void, Ib, Ib, Ib, Ib]:(replicate 17 [])) (Level 10 500) [] 0
    windowSize $= Size windowHeight windowWidth
    viewport $= (Position 0 0, Size windowWidth windowHeight)
    displayCallback $= displayGame game
    reshapeCallback $= Just reshapeHandler
    keyboardMouseCallback $= Just (keyboardHandler queue paused)

    --addTimerCallback 500 (dropOne game)
    idleCallback $= Just (menuHandler queue game)

    fullScreen
    mainLoop

reshapeHandler :: ReshapeCallback
reshapeHandler size = do
    let Size w h = size
        newsize :: GLsizei
        newsize  = min w h
        offsetY :: GLint
        offsetY  = (h - newsize)
    viewport    $= (Position 0 offsetY, Size newsize newsize)
    postRedisplay Nothing

gameIdleHandler :: TVar [Key] -> TVar Game -> IdleCallback
gameIdleHandler queue game = do
    input <- readInput queue
    case input of
        Just (Char '\ESC') -> do                                                -- return to menu
            atomically $ writeTVar game $ GameMenu Start
            idleCallback $= Just (menuHandler queue game)
            postRedisplay Nothing
        Just (Char '\r')   -> modifyAndDisplay game togglePause
        Just (Char '\n')   -> modifyAndDisplay game togglePause
        Just (Char 'p' )   -> modifyAndDisplay game togglePause
        _                  -> return ()
    where togglePause g@(Game _ _ _ _ p) = g { paused = not p }
          togglePause m                  = m

menuHandler :: TVar [Key] -> TVar Game -> IdleCallback
menuHandler queue game = do
    input <- readInput queue
    case input of
        Just (Char '\ESC')        -> exitSuccess
        Just (SpecialKey KeyUp)   -> modifyAndDisplay game toggleMenuItem
        Just (SpecialKey KeyDown) -> modifyAndDisplay game toggleMenuItem
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
                GameMenu Start         -> do
                    atomically $ writeTVar game $ Game ([Void, Ib, Ib, Ib, Ib]:(replicate 17 [])) (Level 10 500) [] 0 False
                    idleCallback $= Just (gameIdleHandler queue game)
                g@(Game _ _ _ _ True)  -> atomically $ writeTVar game $ g { paused = False}
                g@(Game _ _ _ _ False) -> atomically $ writeTVar game $ g { paused = True }

modifyAndDisplay :: TVar Game -> (Game -> Game) -> IO ()
modifyAndDisplay game action = do
    atomically $ modifyTVar game action
    postRedisplay Nothing

readInput :: TVar [Key] -> IO (Maybe Key)
readInput queue = atomically $ do
    q <- readTVar queue
    case q of
        []    -> return Nothing
        (h:t) -> do
            writeTVar queue t
            return $ Just h

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

