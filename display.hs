module Display where

import Control.Concurrent.STM
import Control.Applicative
import Graphics.UI.GLUT

import Util


displayGame :: TVar Game -> DisplayCallback
displayGame game = do
    clear [ColorBuffer]
    drawBackdrop
    preservingMatrix $ do
        -- set frame of reference for drawing the field
        translate $ Vector3 (-8/9 :: GLfloat) (-1 :: GLfloat) (0 :: GLfloat)
        scale (0.1 :: GLfloat) (2/18 :: GLfloat) (1 :: GLfloat)
        w <- world <$> (atomically $ readTVar game)
        drawWorld w
    flush

drawWorld :: World -> IO ()
drawWorld (World world) = do
    drawLines 0 world
    where drawLines _ []   = return ()
          drawLines y (l:t)    = do
            drawCells 0 y l
            drawLines (y+1) t
          drawCells _ _ [] = return ()
          drawCells x y (h:t) = do
            drawBlockAt h x y
            drawCells (x+1) y t

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
            print heightOfString
            print widthOfString
            renderString TimesRoman10 "Hello World"

drawBlockAt :: Block -> GLfloat -> GLfloat -> IO ()
drawBlockAt block x y = do
    preservingMatrix $ do
        translate $ Vector3 x y (0::GLfloat)
        drawBlock $ colorScheme block

drawBlock :: (IO (), IO (), IO (), IO ()) -> IO ()
drawBlock (c1,c2,c3,c4) = renderPrimitive Quads $ do
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
