module Display where

import Control.Concurrent.STM
import Control.Applicative
import Graphics.UI.GLUT hiding (Level)


import Util

displayGame :: TVar Game -> DisplayCallback
displayGame tGame = do
    clear [ColorBuffer]
    g <- atomically $ readTVar tGame
    case g of
        GameMenu i     -> drawMenu i
        Game _ _ _ _ _ -> do
            drawBackdrop
            preservingMatrix $ do
                -- set frame of reference for drawing the field
                translate $ Vector3 (-8/9 :: GLfloat) (1 :: GLfloat) (0 :: GLfloat)
                scale (0.1 :: GLfloat) (2/18 :: GLfloat) (1 :: GLfloat)
                drawWorld $ world g
            if paused g then drawPausedOverlay else return ()
    flush

drawWorld :: World -> IO ()
drawWorld world = do
    drawLines 0 world
    where drawLines _ []   = return ()
          drawLines y (l:t)    = do
            drawCells 0 y l
            drawLines (y+1) t
          drawCells _ _ [] = return ()
          drawCells x y (h:t) = do
            drawBlockAt h x y
            drawCells (x+1) y t

drawText :: Game -> IO ()
drawText game = do
    preservingMatrix $ do
        renderString TimesRoman10 $ show $ score game


drawPausedOverlay :: IO ()
drawPausedOverlay = renderPrimitive Quads $ do
    black
    vertex $ Vertex3 (-0.51 :: GLfloat) ( 0.26 :: GLfloat) (0 :: GLfloat)
    vertex $ Vertex3 (-0.51 :: GLfloat) (-0.26 :: GLfloat) (0 :: GLfloat)
    vertex $ Vertex3 ( 0.51 :: GLfloat) (-0.26 :: GLfloat) (0 :: GLfloat)
    vertex $ Vertex3 ( 0.51 :: GLfloat) ( 0.26 :: GLfloat) (0 :: GLfloat)
    white
    vertex $ Vertex3 (-0.5 :: GLfloat) ( 0.25 :: GLfloat) (0 :: GLfloat)
    vertex $ Vertex3 (-0.5 :: GLfloat) (-0.25 :: GLfloat) (0 :: GLfloat)
    vertex $ Vertex3 ( 0.5 :: GLfloat) (-0.25 :: GLfloat) (0 :: GLfloat)
    vertex $ Vertex3 ( 0.5 :: GLfloat) ( 0.25 :: GLfloat) (0 :: GLfloat)

drawBackdrop :: IO ()
drawBackdrop = do
    renderPrimitive Quads $ do
        -- playing field
        white
        vertex $ Vertex3 (-8/9  :: GLfloat) ( 1     :: GLfloat) (0 :: GLfloat)
        vertex $ Vertex3 ( 1/9  :: GLfloat) ( 1     :: GLfloat) (0 :: GLfloat)
        vertex $ Vertex3 ( 1/9  :: GLfloat) (-1     :: GLfloat) (0 :: GLfloat)
        vertex $ Vertex3 (-8/9  :: GLfloat) (-1     :: GLfloat) (0 :: GLfloat)
        -- lines until next level
        vertex $ Vertex3 (0.25  :: GLfloat) (0.125  :: GLfloat) (0 :: GLfloat)
        vertex $ Vertex3 (0.875 :: GLfloat) (0.125  :: GLfloat) (0 :: GLfloat)
        vertex $ Vertex3 (0.875 :: GLfloat) (0.375  :: GLfloat) (0 :: GLfloat)
        vertex $ Vertex3 (0.25  :: GLfloat) (0.375  :: GLfloat) (0 :: GLfloat)
        -- score
        vertex $ Vertex3 (0.25  :: GLfloat) (0.625  :: GLfloat) (0 :: GLfloat)
        vertex $ Vertex3 (0.875 :: GLfloat) (0.625  :: GLfloat) (0 :: GLfloat)
        vertex $ Vertex3 (0.875 :: GLfloat) (0.875  :: GLfloat) (0 :: GLfloat)
        vertex $ Vertex3 (0.25  :: GLfloat) (0.875  :: GLfloat) (0 :: GLfloat)
        -- next tile
        vertex $ Vertex3 (0.25  :: GLfloat) (-0.25  :: GLfloat) (0 :: GLfloat)
        vertex $ Vertex3 (0.875 :: GLfloat) (-0.25  :: GLfloat) (0 :: GLfloat)
        vertex $ Vertex3 (0.875 :: GLfloat) (-0.875 :: GLfloat) (0 :: GLfloat)
        vertex $ Vertex3 (0.25  :: GLfloat) (-0.875 :: GLfloat) (0 :: GLfloat)

    preservingMatrix $ do
        scale (2 :: GLfloat) (2 :: GLfloat) (1 :: GLfloat)
        lightG
        heightOfString <- fontHeight TimesRoman10
        widthOfString <- stringWidth TimesRoman10 "HelloWorld"
        renderString TimesRoman10 "Hello World"

drawBlockAt :: Tetromino t => t -> GLfloat -> GLfloat -> IO ()
drawBlockAt block x y = do
    preservingMatrix $ do
        translate $ Vector3 x (y - 18) (0::GLfloat)
        drawBlock block

drawBlock :: Tetromino t => t -> IO ()
drawBlock block = renderPrimitive Quads $ do
    c1
    vertex $ Vertex3 (0    :: GLfloat) (0    :: GLfloat) (0 :: GLfloat)
    vertex $ Vertex3 (0    :: GLfloat) (1    :: GLfloat) (0 :: GLfloat)
    vertex $ Vertex3 (1    :: GLfloat) (1    :: GLfloat) (0 :: GLfloat)
    vertex $ Vertex3 (1    :: GLfloat) (0    :: GLfloat) (0 :: GLfloat)
    c2
    vertex $ Vertex3 (0.06 :: GLfloat) (0.06 :: GLfloat) (0 :: GLfloat)
    vertex $ Vertex3 (0.06 :: GLfloat) (0.94 :: GLfloat) (0 :: GLfloat)
    vertex $ Vertex3 (0.94 :: GLfloat) (0.94 :: GLfloat) (0 :: GLfloat)
    vertex $ Vertex3 (0.94 :: GLfloat) (0.06 :: GLfloat) (0 :: GLfloat)
    c3
    vertex $ Vertex3 (0.32 :: GLfloat) (0.32 :: GLfloat) (0 :: GLfloat)
    vertex $ Vertex3 (0.32 :: GLfloat) (0.68 :: GLfloat) (0 :: GLfloat)
    vertex $ Vertex3 (0.68 :: GLfloat) (0.68 :: GLfloat) (0 :: GLfloat)
    vertex $ Vertex3 (0.68 :: GLfloat) (0.32 :: GLfloat) (0 :: GLfloat)
    c4
    vertex $ Vertex3 (0.4  :: GLfloat) (0.4  :: GLfloat) (0 :: GLfloat)
    vertex $ Vertex3 (0.4  :: GLfloat) (0.6  :: GLfloat) (0 :: GLfloat)
    vertex $ Vertex3 (0.6  :: GLfloat) (0.6  :: GLfloat) (0 :: GLfloat)
    vertex $ Vertex3 (0.6  :: GLfloat) (0.4  :: GLfloat) (0 :: GLfloat)
    where (c1, c2, c3, c4) = colorScheme block


drawMenu :: MItem -> IO ()
drawMenu active = do
    renderPrimitive Quads $ do
        lightG
        case active of
            Start -> do
                vertex $ Vertex3 (-0.52 :: GLfloat) (-0.53  :: GLfloat) (0 :: GLfloat)
                vertex $ Vertex3 ( 0.52 :: GLfloat) (-0.53  :: GLfloat) (0 :: GLfloat)
                vertex $ Vertex3 ( 0.52 :: GLfloat) (-0.695 :: GLfloat) (0 :: GLfloat)
                vertex $ Vertex3 (-0.52 :: GLfloat) (-0.695 :: GLfloat) (0 :: GLfloat)
            Quit  -> do
                vertex $ Vertex3 (-0.52 :: GLfloat) (-0.73  :: GLfloat) (0 :: GLfloat)
                vertex $ Vertex3 ( 0.52 :: GLfloat) (-0.73  :: GLfloat) (0 :: GLfloat)
                vertex $ Vertex3 ( 0.52 :: GLfloat) (-0.895 :: GLfloat) (0 :: GLfloat)
                vertex $ Vertex3 (-0.52 :: GLfloat) (-0.895 :: GLfloat) (0 :: GLfloat)
        black
        vertex $ Vertex3 (-0.51 :: GLfloat) (-0.74  :: GLfloat) (0 :: GLfloat)
        vertex $ Vertex3 ( 0.51 :: GLfloat) (-0.74  :: GLfloat) (0 :: GLfloat)
        vertex $ Vertex3 ( 0.51 :: GLfloat) (-0.885 :: GLfloat) (0 :: GLfloat)
        vertex $ Vertex3 (-0.51 :: GLfloat) (-0.885 :: GLfloat) (0 :: GLfloat)

        vertex $ Vertex3 (-0.51 :: GLfloat) (-0.54  :: GLfloat) (0 :: GLfloat)
        vertex $ Vertex3 ( 0.51 :: GLfloat) (-0.54  :: GLfloat) (0 :: GLfloat)
        vertex $ Vertex3 ( 0.51 :: GLfloat) (-0.685 :: GLfloat) (0 :: GLfloat)
        vertex $ Vertex3 (-0.51 :: GLfloat) (-0.685 :: GLfloat) (0 :: GLfloat)

        white
        vertex $ Vertex3 (-0.50 :: GLfloat) (-0.75  :: GLfloat) (0 :: GLfloat)
        vertex $ Vertex3 ( 0.50 :: GLfloat) (-0.75  :: GLfloat) (0 :: GLfloat)
        vertex $ Vertex3 ( 0.50 :: GLfloat) (-0.875 :: GLfloat) (0 :: GLfloat)
        vertex $ Vertex3 (-0.50 :: GLfloat) (-0.875 :: GLfloat) (0 :: GLfloat)

        vertex $ Vertex3 (-0.50 :: GLfloat) (-0.55  :: GLfloat) (0 :: GLfloat)
        vertex $ Vertex3 ( 0.50 :: GLfloat) (-0.55  :: GLfloat) (0 :: GLfloat)
        vertex $ Vertex3 ( 0.50 :: GLfloat) (-0.675 :: GLfloat) (0 :: GLfloat)
        vertex $ Vertex3 (-0.50 :: GLfloat) (-0.675 :: GLfloat) (0 :: GLfloat)

