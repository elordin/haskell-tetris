{-# LANGUAGE TemplateHaskell #-}

import Control.Concurrent.STM
import Control.Concurrent
import Graphics.UI.GLUT hiding (Level)
import Control.Applicative
import System.Exit
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
        Just (Char '\ESC') -> do                                                -- return to menu
            atomically $ writeTVar game $ GameMenu Start
            addTimerCallback 16 (menuHandler queue game)
            postRedisplay Nothing
        Just (Char '\r')   -> do
            modifyAndDisplay game togglePause
            addTimerCallback 16 (gameHandler queue game)
        Just (Char '\n')   -> do
            modifyAndDisplay game togglePause
            addTimerCallback 16 (gameHandler queue game)
        Just (Char 'p' )   -> do
            modifyAndDisplay game togglePause
            addTimerCallback 16 (gameHandler queue game)
        Just (SpecialKey KeyLeft) -> do
            modifyAndDisplay game (\g -> shiftLeft g)
            addTimerCallback 16 (gameHandler queue game)
        Just (SpecialKey KeyRight) -> do
            modifyAndDisplay game (\g -> shiftRight g)
            addTimerCallback 16 (gameHandler queue game)
        Just (SpecialKey KeyDown) -> do
            modifyAndDisplay game (\g -> shiftDown g)
            addTimerCallback 16 (gameHandler queue game)
        Just (SpecialKey KeyUp) -> do
            modifyAndDisplay game (\g -> spin g)
            addTimerCallback 16 (gameHandler queue game)
        _                  -> 
            addTimerCallback 16 (gameHandler queue game)
    where togglePause g@(Game _ _ _ _ p _ _) = g { paused = not p }
          togglePause m                  = m

shiftLeft :: Game -> Game
shiftLeft game = shiftBlockH game (+(-1))

shiftRight :: Game -> Game
shiftRight game = shiftBlockH game (+1)

shiftDown :: Game -> Game
shiftDown game = shiftBlockV game (+(-1))

shiftUp :: Game -> Game
shiftUp game = shiftBlockV game (+1)

shiftBlockH :: Game -> (Int -> Int) -> Game
shiftBlockH game op = shiftBlock game op id

shiftBlockV :: Game -> (Int -> Int) -> Game
shiftBlockV game op = shiftBlock game id op

shiftBlock :: Game -> (Int -> Int) -> (Int -> Int) -> Game
shiftBlock game@(Game w l ls s p h (blockType, ((x1,y1),(x2,y2),(x3,y3),(x4,y4)))) opX opY = 
    let newPos = ((opX x1, opY y1),(opX x2, opY y2),(opX x3, opY y3),(opX x4, opY y4))
    in if freeForBlock game newPos
       then Game w l ls s p h (blockType, newPos)
       else game

freeForBlock :: Game -> (Coord, Coord, Coord, Coord) -> Bool
freeForBlock game (a,b,c,d) =  
    and $ map (\(x,y) -> x >= 0 
        && x < 10 
        && y >= 0 
        && y < 17 
        && case Map.lookup (x,y) $ world game of
            Nothing   -> True
            Just Void -> True
            _         -> False) [a,b,c,d]

spin :: Game -> Game
spin game@(Game w l ls s p h (blockType, coords@(cor,_,_,_))) = 
    let rotated = rotateNormalized coords cor Clockwise
    in if freeForBlock game rotated 
       then Game w l ls s p h (blockType, rotated)
       else game
spin g = g

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
                g@(Game _ _ _ _ True  _ _)  -> do
                    atomically $ writeTVar game $ g { paused = False}
                    addTimerCallback 16 (gameHandler queue game)
                g@(Game _ _ _ _ False _ _) -> do 
                    atomically $ writeTVar game $ g { paused = True }
                    addTimerCallback 16 (gameHandler queue game)

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
