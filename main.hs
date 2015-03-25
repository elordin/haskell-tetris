import Control.Concurrent.STM (atomically, TVar, readTVar, modifyTVar, writeTVar, newTVarIO)
import Graphics.UI.GLUT (Key(..), SpecialKey(..), Size(..), Position(..), GLsizei, GLint,
    ($=), createWindow, initialWindowSize, getArgsAndInitialize, postRedisplay, addTimerCallback,
    displayCallback, reshapeCallback, keyboardMouseCallback, mainLoop, viewport, fullScreen)
import System.Exit (exitSuccess)
import System.Random (getStdRandom, randomR)

import Util
import Game
import KeyboardHandler
import Display

main :: IO ()
main = do
    initialWindowSize     $= Size windowWidth windowHeight
    getArgsAndInitialize
    window                <- createWindow "Tetris"
    queue                 <- newTVarIO []
    game                  <- newTVarIO $ GameMenu Start
    viewport              $= (Position 0 0, Size windowWidth windowHeight)
    displayCallback       $= displayGame game
    reshapeCallback       $= Just reshapeHandler
    keyboardMouseCallback $= Just (keyboardHandler queue)
    addTimerCallback 10 (menuHandler queue game)
    fullScreen
    mainLoop

-- Helpers
-- -----------------------------------------------------------------------------
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

-- WINDOW HANDLERS
-- -----------------------------------------------------------------------------
reshapeHandler :: Size -> IO ()
reshapeHandler size = do
    let Size w h = size
        newsize :: GLsizei
        newsize  = min w h
        offsetY :: GLint
        offsetY  = h - newsize
    viewport    $= (Position (floor (fromIntegral (max (w - newsize) 0) / 2)) offsetY, Size newsize newsize)
    postRedisplay Nothing

-- Handles keyboard input during the game
gameHandler :: TVar [Key] -> TVar Game -> IO ()
gameHandler queue game = do
    input <- readInput queue
    case input of
        Just (Char '\ESC')          -> do -- return to menu
            atomically $ writeTVar game $ GameMenu Start
            addTimerCallback 10 (menuHandler queue game)
            postRedisplay Nothing
        Just (Char '\r')            -> pauseOrReturn
        Just (Char '\n')            -> pauseOrReturn
        Just (Char 'p' )            -> modAndPlay togglePause
        Just (SpecialKey KeyLeft)   -> modAndPlay shiftLeft
        Just (SpecialKey KeyRight)  -> modAndPlay shiftRight
        Just (SpecialKey KeyDown)   -> modAndPlay shiftDown
        Just (SpecialKey KeyUp)     -> modAndPlay spin
        Just (Char ' ')             -> modAndPlay dropDown
        Just (SpecialKey KeyShiftR) -> modAndPlay holdBlock
        _                           ->
            addTimerCallback 10 (gameHandler queue game)
    where pauseOrReturn = do
            g <- atomically $ readTVar game
            case g of
               Game {} -> modAndPlay togglePause
               _                    -> do
                    atomically $ writeTVar game $ GameMenu Start
                    postRedisplay Nothing
                    addTimerCallback 10 (menuHandler queue game)
          modAndPlay modifier = do
            modifyAndDisplay game modifier
            addTimerCallback 10 (gameHandler queue game)

-- Handles the periodically dropping of the active block
dropHandler :: TVar Game -> IO ()
dropHandler game = do
    f <- atomically $ do
        g <- readTVar game
        case g of
            g@(Game w ls _ _ _ _ (_,((x1,y1), (x2,y2), (x3, y3), (x4,y4))) _) -> do
                if freeForBlock w ((x1,y1-1), (x2,y2-1), (x3,y3-1), (x4,y4-1))
                then modifyTVar game shiftDown
                else modifyTVar game placeAndNew
                case ls of
                    []                 -> do
                        writeTVar game $ GameError "Out of levels."
                        return (-1)
                    (Level _ freq :t) -> return freq
            _ -> return (-1)
    if f > 0
    then do
        addTimerCallback f (dropHandler game)
        postRedisplay Nothing
    else return ()

-- Handles menu item selection
menuHandler :: TVar [Key] -> TVar Game -> IO ()
menuHandler queue game = do
    input <- readInput queue
    case input of
        Just (Char '\ESC')        -> exitSuccess
        Just (SpecialKey KeyUp)   -> toggleMenuItem
        Just (SpecialKey KeyDown) -> toggleMenuItem
        Just (Char '\n')          -> select
        Just (Char '\r')          -> select
        _                         -> addTimerCallback 10 (menuHandler queue game)
    where toggleMenuItem = do
            atomically $
                modifyTVar game
                    (\g -> case g of
                            GameMenu Start -> GameMenu Quit
                            GameMenu Quit  -> GameMenu Start
                            other          -> other)
            postRedisplay Nothing
            addTimerCallback 10 (menuHandler queue game)
          select = do
            g <- atomically $ readTVar game
            case g of
                GameMenu Quit          -> exitSuccess
                GameMenu Start         -> do
                    seed <- getStdRandom (randomR (1, 10000))
                    let newGame = defaultNewGame seed
                    atomically $ writeTVar game newGame
                    addTimerCallback 10 (gameHandler queue game)
                    case levels newGame of
                        []              -> return ()
                        ((Level _ f):t) -> addTimerCallback f (dropHandler game)
                g@(Game _ _ _ True _ _ _ _)  -> do
                    atomically $ writeTVar game $ g { paused = False}
                    addTimerCallback 10 (gameHandler queue game)
                g@(Game _ _ _ False _ _ _ _) -> do
                    atomically $ writeTVar game $ g { paused = True }
                    addTimerCallback 10 (gameHandler queue game)
                other -> do
                    atomically $ writeTVar game $ GameMenu Start
                    addTimerCallback 10 (menuHandler queue game)
            postRedisplay Nothing
