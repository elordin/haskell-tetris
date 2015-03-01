module Game where

import Control.Concurrent.STM
import Graphics.UI.GLUT hiding (Level)
import qualified Data.Map.Strict as Map
import System.Random

--import KeyboardHandler
--import Display
import Util

togglePause :: Game -> Game
togglePause g@(Game _ _ _ p _ _ _) = g { paused = not p }
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
shiftBlock game@(Game w ls s p h n (blockType, ((x1,y1),(x2,y2),(x3,y3),(x4,y4)))) opX opY = 
    let newPos = ((opX x1, opY y1),(opX x2, opY y2),(opX x3, opY y3),(opX x4, opY y4))
    in if not p && freeForBlock game newPos
       then Game w ls s p h n (blockType, newPos)
       else game

freeForBlock :: Game -> (Coord, Coord, Coord, Coord) -> Bool
freeForBlock (Game w _ _ _ _ _ _) (a,b,c,d) =  
    and $ map (\(x,y) -> x >= 0 
        && x < 10 
        && y >= 0 
        && y < 18 
        && case Map.lookup (x,y) w of
            Nothing   -> True
            Just b    -> case coords b of 
                Nothing -> True
                Just _  -> False) [a,b,c,d]

spin :: Game -> Game
spin game@(Game w ls s p h n (blockType, coords@(cor, _, _, _))) =
    if (not p) && (freeForBlock game rotated) 
    then Game w ls s p h n (blockType, rotated)
    else game
    where rotated = rotateNormalized coords cor Clockwise
spin m = m

dropDown :: Game -> Game
dropDown = (foldl (\f _ -> shiftDown.f) shiftDown [1..17])

holdBlock :: Game -> Game
holdBlock game@(Game w ls s p (h,ch) n (a,_)) = 
    if p || not ch 
    then game 
    else case h of
            Nothing -> game
            Just ho -> 
                case coords ho of 
                    Nothing -> game
                    Just ((x1,y1), (x2,y2), (x3, y3), (x4,y4)) -> 
                        Game w ls s p (Just a, False) n (ho, ( (x1+4,y1+16) 
                                                      , (x2+4,y2+16)
                                                      , (x3+4,y3+16)
                                                      , (x4+4,y4+16) ) )

placeAndNew :: Game -> Game
placeAndNew game = game
    -- place activeBlock in world
    -- remove complete lines and increase score
    -- get new piece and 

--clearFullLines :: Game -> Game 