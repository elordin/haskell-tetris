module Display where

import Control.Concurrent.STM (atomically, TVar, readTVar)
import Graphics.UI.GLUT (GLfloat, Vector3(..), PrimitiveMode(..), ClearBuffer(..),
    scale, translate, preservingMatrix, renderPrimitive, clear, flush)
import Data.Map.Strict (toList)

import Util
import Font
import Game (freeForBlock)

-- displays running game or menu when given respectively
displayGame :: TVar Game -> IO ()
displayGame tGame = do
    clear [ColorBuffer]
    g <- atomically $ readTVar tGame
    case g of
        GameError e    -> drawTextAt ((-0.05) * fromIntegral (length e), 0, 0) (0.1, 0.1, 0.1) e red
        GameOver score -> do
            drawTextAt (-0.4,0.4,0) (0.2,0.2,1) "Game" lightG
            drawTextAt (-0.4,0.2,0) (0.2,0.2,1) "Over" lightG
            drawTextAt (-0.25,0,0) (0.1,0.1,1) "Score" white
            drawTextAt (-0.05 * fromIntegral (length $ show score), -0.1, 0) (0.1, 0.1, 0.1) (show score) white
        GameMenu i     -> drawMenu i
        Game w ls s p (h, ch) nxtBlock (blockType, cs) _ -> do
            drawBackdrop
            -- draw score and lines2go text
            drawLinesToGo $ case ls of { [] -> 0; (Level l _ : t) -> l }
            drawScore s
            -- redraw hold area in gray if already held a block
            if ch
            then return ()
            else renderPrimitive Quads $ do
                gray
                mapV3 [(0.25, 0.125, 0), (0.875, 0.125, 0), (0.875, -0.3, 0), (0.25, -0.3, 0)]
            if p
            then drawPausedOverlay
            else preservingMatrix $ do
                -- set frame of reference for drawing the field
                translate $ Vector3 (-8/9 :: GLfloat) (1 :: GLfloat) (0 :: GLfloat)
                scale (1/fromIntegral blocksX :: GLfloat) (2/fromIntegral blocksY :: GLfloat) (1 :: GLfloat)
                -- draw world
                drawWorld w
                -- draw active block with ghost
                drawGhost w cs
                drawSingle blockType cs

                -- TODO: make independent of world scale for worlds of different sizes
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

-- draws a single character in a 1 x 1 square
drawChar :: Char -> IO ()
drawChar c =
    preservingMatrix $ do
        translate $ Vector3 (0.1 :: GLfloat) (0.1 :: GLfloat) (0 :: GLfloat)
        scale (0.8 :: GLfloat) (0.8 :: GLfloat) (0 :: GLfloat)
        renderPrimitive TriangleStrip $ mapV3 $ letterPath c

-- draws a String in a 1 x String-length rectangle
drawText :: String -> IO ()
drawText = mapM_ (\c -> do
    drawChar c
    translate $ Vector3 (1::GLfloat) (0::GLfloat) (0::GLfloat))

-- draws a String at a given position at a give scale
drawTextAt :: (GLfloat, GLfloat, GLfloat) -> (GLfloat, GLfloat, GLfloat) -> String -> IO () -> IO ()
drawTextAt (x,y,z) (sx,sy,sz) text colour = preservingMatrix $ do
    colour
    translate $ Vector3 x y z
    scale sx sy sz
    drawText text

-- draws number indicating remaining lines
drawLinesToGo :: Int -> IO ()
drawLinesToGo lines2go =
    drawTextAt (0.3125 + 0.1 * (fromIntegral $ 5 - length (show lines2go)), 0.325, 0) (0.1, 0.1, 0.1) (show lines2go) black

-- draws number indicating the score
drawScore :: Int -> IO ()
drawScore score =
    drawTextAt (0.3125 + 0.1 * (fromIntegral $ 5 - length (show score)), 0.7, 0) (0.1, 0.1, 0.1) (show score) black

-- draws a single Block in a 1 x 1 square
drawBlock :: Tetromino t => t -> IO ()
drawBlock block = renderPrimitive Quads $ do
    c1
    mapV3 [(0, 0, 0), (0, 1, 0), (1, 1, 0), (1, 0, 0)]
    c2
    mapV3 [(0.06, 0.06, 0), (0.06, 0.94, 0), (0.94, 0.94, 0), (0.94, 0.06, 0)]
    c3
    mapV3 [(0.32, 0.32, 0), (0.32, 0.68, 0), (0.68, 0.68, 0), (0.68, 0.32, 0)]
    c4
    mapV3 [(0.4, 0.4, 0), (0.4, 0.6, 0), (0.6, 0.6, 0), (0.6, 0.4, 0)]
    where (c1, c2, c3, c4) = colorScheme block

-- draws a single Block in a 1 x 1 square at a given position
drawBlockAt :: Tetromino t => t -> GLfloat -> GLfloat -> IO ()
drawBlockAt block x y =
    preservingMatrix $ do
        translate $ Vector3 x (y - fromIntegral blocksY) (0::GLfloat)
        drawBlock block

-- draws a ghost-block in a 1 x 1 square
drawGhost :: Tetromino t => World t -> (Coord, Coord, Coord, Coord) -> IO ()
drawGhost w (c1@(x1,y1),c2@(x2,y2),c3@(x3,y3),c4@(x4,y4))
    | freeForBlock w next = drawGhost w next
    | otherwise = mapM_ (\(x,y) -> drawGhostBlockAt (fromIntegral x :: GLfloat) (fromIntegral y :: GLfloat)) [c1,c2,c3,c4]
    where next = ((x1,y1-1),(x2,y2-1),(x3,y3-1),(x4,y4-1))

-- draws a ghost-block at a given position
drawGhostBlockAt :: GLfloat -> GLfloat -> IO ()
drawGhostBlockAt x y =
    preservingMatrix $ do
        translate $ Vector3 x (y - fromIntegral blocksY) (0::GLfloat)
        renderPrimitive Quads $ do
            gray
            mapV3 [(0.06, 0.06, 0), (0.06, 0.94, 0), (0.94, 0.94, 0), (0.94, 0.06, 0)]
            white
            mapV3 [(0.16, 0.16, 0), (0.16, 0.84, 0), (0.84, 0.84, 0), (0.84, 0.16, 0)]

-- draws black and white game backdrop
drawBackdrop :: IO ()
drawBackdrop = do
    renderPrimitive Quads $ do
        white
        mapV3 [
            -- playing field
              (-8/9, 1, 0), (1/9, 1, 0), (1/9, -1, 0), (-8/9, -1, 0)
            -- lines for level up area
            , (0.25, 0.25, 0), (0.875, 0.25, 0), (0.875, 0.5, 0), (0.25, 0.5, 0)
            -- score area
            , (0.25, 0.625, 0), (0.875, 0.625, 0), (0.875, 0.875, 0), (0.25, 0.875, 0)
            -- hold area
            , (0.25, 0.125, 0), (0.875, 0.125, 0), (0.875, -0.3, 0), (0.25, -0.3, 0)
            -- next tile area
            , (0.25, -0.425, 0), (0.875, -0.425, 0), (0.875, -0.85, 0), (0.25, -0.85, 0)]
    drawCaptionAt 0.875 "Score"
    drawCaptionAt 0.5 "Lines"
    drawCaptionAt 0.125 "Hold"
    drawCaptionAt (-0.425) "Next"
    where drawCaptionAt y caption =
            preservingMatrix $ do
                translate $ Vector3 (0.25 :: GLfloat) (y :: GLfloat) (0 :: GLfloat)
                scale (0.05 :: GLfloat) (0.05 :: GLfloat) (1 :: GLfloat)
                drawText caption

-- draws game world with each block being 1 x 1 large
drawWorld :: Tetromino t => World t -> IO ()
drawWorld world =
    mapM_ (\((x,y),b) -> drawBlockAt b (fromIntegral x) (fromIntegral y)) $ toList world

-- draws pause overlay at the screens centre
drawPausedOverlay :: IO ()
drawPausedOverlay = do
    renderPrimitive Quads $ do
        black
        mapV3 [
            (-0.51, 0.26, 0), (-0.51, -0.26, 0), (0.51, -0.26, 0), (0.51, 0.26, 0)]
        white
        mapV3 [
            (-0.5, 0.25, 0), (-0.5, -0.25, 0), (0.5, -0.25, 0), (0.5, 0.25, 0)]
    black
    preservingMatrix $ do
        translate $ Vector3 (-0.4 :: GLfloat) (0 :: GLfloat) (0 :: GLfloat)
        scale (0.125 :: GLfloat) (0.125 :: GLfloat) (0 :: GLfloat)
        drawText "Paused"

-- draws menu with given menu item selected
drawMenu :: MenuItem -> IO ()
drawMenu active = do
    renderPrimitive Quads $ do
        lightG
        mapV3 $ case active of
            Start -> [ (-0.52, -0.53, 0), (0.52, -0.53, 0), (0.52, -0.695, 0), (-0.52, -0.695, 0) ]
            Quit  -> [ (-0.52, -0.73 , 0), (0.52 , -0.73 , 0), (0.52 , -0.895, 0), (-0.52, -0.895, 0) ]
        black
        mapV3 [ (-0.51, -0.74 , 0), (0.51 , -0.74 , 0), (0.51 , -0.885, 0),
                (-0.51, -0.885, 0), (-0.51, -0.54 , 0), (0.51 , -0.54 , 0),
                (0.51 , -0.685, 0), (-0.51, -0.685, 0)]
        white
        mapV3 [ (-0.50, -0.75 , 0), (0.50 , -0.75 , 0), (0.50 , -0.875, 0),
                (-0.50, -0.875, 0), (-0.50, -0.55 , 0), (0.50 , -0.55 , 0),
                (0.50 , -0.675, 0), (-0.50, -0.675, 0) ]
    drawTextAt (-0.6,0.25,0)     (0.2,0.2,1)     "Tetris"       lightG
    drawTextAt (-0.3,0,0)        (0.05,0.05,1)   "Thomas Weber" white
    drawTextAt (-0.275,-0.1,0)   (0.05,0.05,1)   "FFP WS14/15"  white
    drawTextAt (-0.1875,-0.65,0) (0.075,0.075,1) "Start"        black
    drawTextAt (-0.15,-0.85,0)   (0.075,0.075,1) "Quit"         black
