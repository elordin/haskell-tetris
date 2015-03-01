module Game where

import Control.Concurrent.STM
import Graphics.UI.GLUT hiding (Level)
import qualified Data.Map.Strict as Map

--import KeyboardHandler
--import Display
import Util

togglePause :: Game -> Game
togglePause g@(Game _ _ _ _ p _ _) = g { paused = not p }
togglePause m = m

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
freeForBlock (Game w _ _ _ _ _ _) (a,b,c,d) =  
    and $ map (\(x,y) -> x >= 0 
        && x < 10 
        && y >= 0 
        && y < 17 
        && case Map.lookup (x,y) w of
            Nothing   -> True
            Just b    -> case coords b of 
                Nothing -> True
                Just _  -> False) [a,b,c,d]

--spin :: Game -> Game
--spin game@(Game w l ls s p h (blockType, coords@(cor,_,_,_))) = 
--    let rotated = rotateNormalized coords cor Clockwise
--    in if freeForBlock game rotated 
--       then Game w l ls s p h (blockType, rotated)
--       else game
--spin g = g
