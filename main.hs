import Control.Concurrent.STM
import Graphics.UI.GLUT hiding (Level)
import System.Exit
import System.Random
import qualified Data.Map.Strict as Map

import Util
import Game
import KeyboardHandler
import Display

windowWidth  = 720
windowHeight = 720

main :: IO ()
main = do
    initialWindowSize     $= Size windowWidth windowHeight
    getArgsAndInitialize
    window                <- createWindow "Tretis"
    queue                 <- newTVarIO []
    paused                <- newTVarIO True
    game                  <- newTVarIO $ GameMenu Start
    viewport              $= (Position 0 0, Size windowWidth windowHeight)
    displayCallback       $= displayGame game
    reshapeCallback       $= Just reshapeHandler
    keyboardMouseCallback $= Just (keyboardHandler queue paused)
    addTimerCallback 16 (menuHandler queue game)    
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
reshapeHandler :: ReshapeCallback
reshapeHandler size = do
    let Size w h = size
        newsize :: GLsizei
        newsize  = min w h
        offsetY :: GLint
        offsetY  = (h - newsize)
    viewport    $= (Position (floor $ (fromIntegral (max (w - newsize) 0) / 2)) offsetY, Size newsize newsize)
    postRedisplay Nothing

-- Handles keyboard input during the game
gameHandler :: TVar [Key] -> TVar Game -> IO ()
gameHandler queue game = do
    input <- readInput queue
    case input of
        Just (Char '\ESC')          -> do -- return to menu
            atomically $ writeTVar game $ GameMenu Start
            addTimerCallback 16 (menuHandler queue game)
            postRedisplay Nothing
        Just (Char '\r')            -> modAndPlay togglePause
        Just (Char '\n')            -> modAndPlay togglePause
        Just (Char 'p' )            -> modAndPlay togglePause
        Just (SpecialKey KeyLeft)   -> modAndPlay shiftLeft
        Just (SpecialKey KeyRight)  -> modAndPlay shiftRight
        Just (SpecialKey KeyDown)   -> modAndPlay shiftDown
        Just (SpecialKey KeyUp)     -> modAndPlay spin
        Just (Char ' ')             -> modAndPlay dropDown
        Just (SpecialKey KeyShiftR) -> modAndPlay holdBlock
        _                           ->
            addTimerCallback 16 (gameHandler queue game)
    where modAndPlay :: (Game -> Game) -> IO ()
          modAndPlay modifier = do
            modifyAndDisplay game modifier
            addTimerCallback 16 (gameHandler queue game)

-- Handles the periodically dropping of the active block
dropHandler :: TVar Game -> IO ()
dropHandler game = do
    f <- atomically $ do 
        g <- readTVar game
        case g of
            GameMenu _ -> return (-1)
            g@(Game w ls _ _ _ _ (_,((x1,y1), (x2,y2), (x3, y3), (x4,y4)))) -> do
                if freeForBlock g ((x1,y1-1), (x2,y2-1), (x3,y3-1), (x4,y4-1))
                then modifyTVar game shiftDown
                else modifyTVar game placeAndNew
                case ls of 
                    []                 -> return (-1)
                    ((Level _ freq):t) -> return freq
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
        _                         -> addTimerCallback 16 (menuHandler queue game)
    where toggleMenuItem = do
            atomically $ 
                modifyTVar game 
                    (\g -> case g of
                            GameMenu Start -> GameMenu Quit
                            GameMenu Quit  -> GameMenu Start
                            g              -> g)
            postRedisplay Nothing
            addTimerCallback 16 (menuHandler queue game) 
          select = do
            g <- atomically $ readTVar game
            case g of
                GameMenu Quit          -> exitSuccess
                GameMenu Start         -> do
                    atomically $ writeTVar game defaultNewGame
                    addTimerCallback 16 (gameHandler queue game)
                    case levels defaultNewGame of
                        []              -> return ()
                        ((Level _ f):t) -> addTimerCallback f (dropHandler game)
                g@(Game _ _ _ True _ _ _)  -> do
                    atomically $ writeTVar game $ g { paused = False}
                    addTimerCallback 16 (gameHandler queue game)
                g@(Game _ _ _ False _ _ _) -> do 
                    atomically $ writeTVar game $ g { paused = True }
                    addTimerCallback 16 (gameHandler queue game)
            postRedisplay Nothing