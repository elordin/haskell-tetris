import Control.Concurrent.STM
import Control.Concurrent
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
    window   <- createWindow "Tretis"
    queue    <- newTVarIO []
    paused   <- newTVarIO True
    game     <- newTVarIO $ GameMenu Start
    viewport              $= (Position 0 0, Size windowWidth windowHeight)
    displayCallback       $= displayGame game
    reshapeCallback       $= Just reshapeHandler
    keyboardMouseCallback $= Just (keyboardHandler queue paused)
    addTimerCallback 16 (menuHandler queue game)    
    --fullScreen
    mainLoop

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

gameHandler :: TVar [Key] -> TVar Game -> IO ()
gameHandler queue game = do
    input <- readInput queue
    case input of
        Just (Char '\ESC')          -> do -- return to menu
            atomically $ writeTVar game $ GameMenu Start
            addTimerCallback 16 (menuHandler queue game)
            postRedisplay Nothing
        Just (Char '\r')            -> do
            modifyAndDisplay game togglePause
            addTimerCallback 16 (gameHandler queue game)
        Just (Char '\n')            -> do
            modifyAndDisplay game togglePause
            addTimerCallback 16 (gameHandler queue game)
        Just (Char 'p' )            -> do
            modifyAndDisplay game togglePause
            addTimerCallback 16 (gameHandler queue game)
        Just (SpecialKey KeyLeft)   -> do
            modifyAndDisplay game shiftLeft
            addTimerCallback 16 (gameHandler queue game)
        Just (SpecialKey KeyRight)  -> do
            modifyAndDisplay game shiftRight
            addTimerCallback 16 (gameHandler queue game)
        Just (SpecialKey KeyDown)   -> do
            modifyAndDisplay game shiftDown
            addTimerCallback 16 (gameHandler queue game)
        --Just (SpecialKey KeyUp)     -> do
        --    modifyAndDisplay game spin
        --    addTimerCallback 16 (gameHandler queue game)
        --Just (Char ' ') -> do
        --    modifyAndDisplay game drop
        --    addTimerCallback 16 (gameHandler queue game)
        --Just (SpecialKey KeyShiftR) -> do
        --    modifyAndDisplay game hold
        --    addTimerCallback 16 (gameHandler queue game)
        _                  ->
            addTimerCallback 16 (gameHandler queue game)

dropHandler :: TVar Game -> IO ()
dropHandler game = undefined

menuHandler :: TVar [Key] -> TVar Game -> IO ()
menuHandler queue game = do
    input <- readInput queue
    case input of
        Just (Char '\ESC')        -> exitSuccess
        Just (SpecialKey KeyUp)   -> do modifyAndDisplay game toggleMenuItem
                                        addTimerCallback 16 (menuHandler queue game)
        Just (SpecialKey KeyDown) -> do modifyAndDisplay game toggleMenuItem
                                        addTimerCallback 16 (menuHandler queue game)

        Just (Char '\n')          -> do { select; postRedisplay Nothing }
        Just (Char '\r')          -> do { select; postRedisplay Nothing }
        _                         -> addTimerCallback 16 (menuHandler queue game)
    where toggleMenuItem (GameMenu Start) = GameMenu Quit
          toggleMenuItem (GameMenu Quit)  = GameMenu Start
          toggleMenuItem g = g
          select = do
            g <- atomically $ readTVar game
            case g of
                GameMenu Quit          -> exitSuccess
                GameMenu Start         -> do
                    atomically $ writeTVar game defaultNewGame
                    addTimerCallback 16 (gameHandler queue game)
                    --addTimerCallback (frequency $ level $ defaultNewGame) (dropHandler game)
                g@(Game _ _ _ _ True  _ _)  -> do
                    atomically $ writeTVar game $ g { paused = False}
                    addTimerCallback 16 (gameHandler queue game)
                g@(Game _ _ _ _ False _ _) -> do 
                    atomically $ writeTVar game $ g { paused = True }
                    addTimerCallback 16 (gameHandler queue game)
