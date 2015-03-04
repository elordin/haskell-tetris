module Display where

import Control.Concurrent.STM
import Control.Applicative
import Graphics.UI.GLUT hiding (Level)
import qualified Data.Map.Strict as Map

import Util
import Font

displayGame :: TVar Game -> DisplayCallback
displayGame tGame = do
    clear [ColorBuffer]
    g <- atomically $ readTVar tGame
    case g of
        GameOver       -> return ()
        GameMenu i     -> drawMenu i
        Game w ls s p (h, ch) nxtBlock (blockType, cs) _ -> do
            drawBackdrop

            -- draw score and lines 2 go text
            let lines2go = case ls of 
                            []              -> 0
                            ((Level l _):t) -> l
            preservingMatrix $ do
                black
                translate $ Vector3 ((0.3125 + 0.1 * (fromIntegral $ 5 - (length $ show lines2go))) :: GLfloat) (0.325 :: GLfloat) (0 :: GLfloat)
                scale (0.1 :: GLfloat) (0.1 :: GLfloat) (1 :: GLfloat)
                renderCustomText $ show lines2go
            preservingMatrix $ do
                black
                translate $ Vector3 ((0.3125 + 0.1 * (fromIntegral $ 5 - (length $ show s))) :: GLfloat) (0.7 :: GLfloat) (0 :: GLfloat)
                scale (0.1 :: GLfloat) (0.1 :: GLfloat) (1 :: GLfloat)
                renderCustomText $ show s

            -- redraw hold area in gray if already held a block
            if ch
            then return () 
            else renderPrimitive Quads $ do
                gray
                vertex $ vx3 0.25 0.125 0
                vertex $ vx3 0.875 0.125 0
                vertex $ vx3 0.875 (-0.3) 0
                vertex $ vx3 0.25 (-0.3) 0
            if p 
            then drawPausedOverlay 
            else preservingMatrix $ do
                -- set frame of reference for drawing the field
                translate $ Vector3 (-8/9 :: GLfloat) (1 :: GLfloat) (0 :: GLfloat)
                scale (0.1 :: GLfloat) (2/18 :: GLfloat) (1 :: GLfloat)
                -- draw world
                drawWorld $ w
                -- draw active block
                drawSingle blockType cs
                -- draw hold
                case h of
                    Nothing    -> return ()
                    Just block -> preservingMatrix $ do
                        translate $ Vector3 (14 :: GLfloat) (7.25 :: GLfloat) (0 :: GLfloat)
                        drawSingle block $ coords block
                -- draw next
                preservingMatrix $ do
                    translate $ Vector3 (14 :: GLfloat) (2.25 :: GLfloat) (0 :: GLfloat)
                    drawSingle nxtBlock $ coords nxtBlock
    flush
    where drawSingle blockType (c1,c2,c3,c4) = mapM_ (\(x,y) -> drawBlockAt blockType (fromIntegral x :: GLfloat) (fromIntegral y :: GLfloat)) [c1,c2,c3,c4]

drawWorld :: Tetromino t => World t -> IO ()
drawWorld world = 
    mapM_ (\((x,y),b) -> drawBlockAt b (fromIntegral x) (fromIntegral y)) $ Map.toList world

renderCustomText :: String -> IO ()
renderCustomText str = 
    mapM_ (\c -> do 
        renderChar c
        translate $ Vector3 (1::GLfloat) (0::GLfloat) (0::GLfloat)) $ str

renderChar :: Char -> IO ()
renderChar c = 
    preservingMatrix $ do 
        translate $ Vector3 (0.1 :: GLfloat) (0.1 :: GLfloat) (0 :: GLfloat)  
        scale (0.8 :: GLfloat) (0.8 :: GLfloat) (0 :: GLfloat)  
        renderPrimitive TriangleStrip $ mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) $ letterPath c

drawPausedOverlay :: IO ()
drawPausedOverlay = do
    renderPrimitive Quads $ do
        black
        vertex $ vx3 (-0.51)   0.26  0
        vertex $ vx3 (-0.51) (-0.26) 0
        vertex $ vx3   0.51  (-0.26) 0
        vertex $ vx3   0.51    0.26  0
        white
        vertex $ vx3 (-0.5)   0.25  0
        vertex $ vx3 (-0.5) (-0.25) 0
        vertex $ vx3   0.5  (-0.25) 0
        vertex $ vx3   0.5    0.25  0
    black
    preservingMatrix $ do
        translate $ Vector3 (-0.4 :: GLfloat) (0 :: GLfloat) (0 :: GLfloat)
        scale (0.125 :: GLfloat) (0.125 :: GLfloat) (0 :: GLfloat)
        renderCustomText "Paused"

drawBackdrop :: IO ()
drawBackdrop = do
    renderPrimitive Quads $ do
        -- playing field
        white
        vertex $ vx3 (-8/9)   1 0
        vertex $ vx3 ( 1/9)   1 0
        vertex $ vx3 ( 1/9) (-1) 0
        vertex $ vx3 (-8/9) (-1) 0

        -- lines until next level
        vertex $ vx3 0.25  0.25   0
        vertex $ vx3 0.875 0.25   0
        vertex $ vx3 0.875 0.5    0
        vertex $ vx3 0.25  0.5    0
        -- score
        vertex $ vx3 0.25  0.625  0
        vertex $ vx3 0.875 0.625  0
        vertex $ vx3 0.875 0.875  0
        vertex $ vx3 0.25  0.875  0
        -- hold
        vertex $ vx3 0.25    0.125   0
        vertex $ vx3 0.875   0.125   0
        vertex $ vx3 0.875 (-0.3  )  0
        vertex $ vx3 0.25  (-0.3  )  0
        -- next tile
        vertex $ vx3 0.25  (-0.425) 0
        vertex $ vx3 0.875 (-0.425) 0
        vertex $ vx3 0.875 (-0.85 ) 0
        vertex $ vx3 0.25  (-0.85 ) 0
    drawCaptionAt 0.875 "Score"
    drawCaptionAt 0.5 "Lines"
    drawCaptionAt 0.125 "Hold"
    drawCaptionAt (-0.425) "Next"
    where drawCaptionAt y caption = 
            preservingMatrix $ do 
                translate $ Vector3 (0.25 :: GLfloat) (y :: GLfloat) (0 :: GLfloat)
                scale (0.05 :: GLfloat) (0.05 :: GLfloat) (1 :: GLfloat)
                renderCustomText caption
            
drawBlockAt :: Tetromino t => t -> GLfloat -> GLfloat -> IO ()
drawBlockAt block x y = do
    preservingMatrix $ do
        translate $ Vector3 x (y - 18) (0::GLfloat)
        drawBlock block

drawBlock :: Tetromino t => t -> IO ()
drawBlock block = renderPrimitive Quads $ do
    c1
    vertex $ vx3 0    0    0
    vertex $ vx3 0    1    0
    vertex $ vx3 1    1    0
    vertex $ vx3 1    0    0
    c2
    vertex $ vx3 0.06 0.06 0
    vertex $ vx3 0.06 0.94 0
    vertex $ vx3 0.94 0.94 0
    vertex $ vx3 0.94 0.06 0
    c3
    vertex $ vx3 0.32 0.32 0
    vertex $ vx3 0.32 0.68 0
    vertex $ vx3 0.68 0.68 0
    vertex $ vx3 0.68 0.32 0
    c4
    vertex $ vx3 0.4  0.4  0
    vertex $ vx3 0.4  0.6  0
    vertex $ vx3 0.6  0.6  0
    vertex $ vx3 0.6  0.4  0
    where (c1, c2, c3, c4) = colorScheme block

drawMenu :: MItem -> IO ()
drawMenu active = do
    renderPrimitive Quads $ do
        lightG
        case active of
            Start -> do
                vertex $ vx3 (-0.52) (-0.53)  0
                vertex $ vx3   0.52  (-0.53)  0
                vertex $ vx3   0.52  (-0.695) 0
                vertex $ vx3 (-0.52) (-0.695) 0
            Quit  -> do
                vertex $ vx3 (-0.52) (-0.73 ) 0
                vertex $ vx3   0.52  (-0.73 ) 0
                vertex $ vx3   0.52  (-0.895) 0
                vertex $ vx3 (-0.52) (-0.895) 0
        black
        vertex $ vx3 (-0.51) (-0.74 ) 0
        vertex $ vx3   0.51  (-0.74 ) 0
        vertex $ vx3   0.51  (-0.885) 0
        vertex $ vx3 (-0.51) (-0.885) 0

        vertex $ vx3 (-0.51) (-0.54 ) 0
        vertex $ vx3   0.51  (-0.54 ) 0
        vertex $ vx3   0.51  (-0.685) 0
        vertex $ vx3 (-0.51) (-0.685) 0

        white
        vertex $ vx3 (-0.50) (-0.75 ) 0
        vertex $ vx3   0.50  (-0.75 ) 0
        vertex $ vx3   0.50  (-0.875) 0
        vertex $ vx3 (-0.50) (-0.875) 0

        vertex $ vx3 (-0.50) (-0.55 ) 0
        vertex $ vx3   0.50  (-0.55 ) 0
        vertex $ vx3   0.50  (-0.675) 0
        vertex $ vx3 (-0.50) (-0.675) 0
    preservingMatrix $ do 
        lightG
        translate $ Vector3 (0 :: GLfloat) (0.25::GLfloat) (0::GLfloat)
        scale (0.2 :: GLfloat) (0.2 :: GLfloat) (1 :: GLfloat)
        translate $ Vector3 (-3::GLfloat) (0::GLfloat) (0::GLfloat)
        renderCustomText "Tetris"
    preservingMatrix $ do
        white
        scale (0.05 :: GLfloat) (0.05 :: GLfloat) (1 :: GLfloat)
        translate $ Vector3 (-6::GLfloat) (0::GLfloat) (0::GLfloat)
        renderCustomText "Thomas Weber"
        translate $ Vector3 (-11.5::GLfloat) (-2::GLfloat) (0::GLfloat)
        renderCustomText "FFP WS14/15"
    preservingMatrix $ do
        black
        translate $ Vector3 (0 :: GLfloat) (-0.65::GLfloat) (0::GLfloat)
        scale (0.075 :: GLfloat) (0.075 :: GLfloat) (1 :: GLfloat)
        translate $ Vector3 (-2.5::GLfloat) (0::GLfloat) (0::GLfloat)
        renderCustomText "Start"
    preservingMatrix $ do
        black
        translate $ Vector3 (0 :: GLfloat) (-0.85::GLfloat) (0::GLfloat)
        scale (0.075 :: GLfloat) (0.075 :: GLfloat) (1 :: GLfloat)
        translate $ Vector3 (-2::GLfloat) (0::GLfloat) (0::GLfloat)
        renderCustomText "Quit"