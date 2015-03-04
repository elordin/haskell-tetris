module Game where

import Control.Concurrent.STM
import Graphics.UI.GLUT hiding (Level)
import qualified Data.Map.Strict as Map
import Data.List (sort, sortBy, partition)
import System.Random

import Util

togglePause :: Game -> Game
togglePause g@(Game _ _ _ p _ _ _ _) = g { paused = not p }
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
shiftBlock game@(Game w ls s p h n (blockType, ((x1,y1),(x2,y2),(x3,y3),(x4,y4))) rg) opX opY = 
    let newPos = ((opX x1, opY y1),(opX x2, opY y2),(opX x3, opY y3),(opX x4, opY y4))
    in if not p && freeForBlock game newPos
       then Game w ls s p h n (blockType, newPos) rg
       else game
shiftBlock other _ _ = other

freeForBlock :: Game -> (Coord, Coord, Coord, Coord) -> Bool
freeForBlock (Game w _ _ _ _ _ _ _) (a,b,c,d) =  
    and $ map (\(x,y) -> x >= 0 
        && x < 10 
        && y >= 0 
        && y < 18 
        && case Map.lookup (x,y) w of
            Nothing   -> True
            Just b    -> False) [a,b,c,d]
freeForBlock _ _ = False

spin :: Game -> Game
spin game@(Game w ls s p h n (blockType, coords@(cor, _, _, _)) rg) =
    if (not p) && (freeForBlock game rotated) 
    then Game w ls s p h n (blockType, rotated) rg
    else game
    where rotated = rotateNormalized coords cor Clockwise
spin other = other

dropDown :: Game -> Game
dropDown = (foldl (\f _ -> shiftDown.f) shiftDown [1..17])

holdBlock :: Game -> Game
holdBlock game@(Game w ls s p (h,ch) n (a,_) rg) = 
    if p || not ch 
    then game 
    else case h of
            Nothing -> game
            Just ho -> Game w ls s p (Just a, False) n (ho, pushToTop $ coords ho) rg
holdBlock other = other

placeAndNew :: Game -> Game
placeAndNew g@(Game _ [] _ _ _ _ _ _) = GameError "Out of levels."
placeAndNew g@(Game _ _ _ _ _ _ _ []) = GameError "Out of random numbers." 
placeAndNew g@(Game w ((Level l f):ls) s p (h,ch) nb ab (rnd:rnds)) =
    if freeForBlock g nextBlockPos
    then Game newWorld newLevels (s + (scorePerLines completeLines)) p (h,True) newRandomBlock (nb, nextBlockPos) rnds
    else GameOver
    where (newWorld, completeLines) = clearOutCompleteLines $ insertToWorld w ab
          clearOutCompleteLines :: Tetromino t => World t -> (World t, Int)
          clearOutCompleteLines targetWorld = (newWorld $ newWorldUnshifted targetWorld, length indicesOfCompleteLines)
                where lineRemaining :: [(Int, Bool)]
                      lineRemaining = map (\i -> (i, (Map.size $ Map.filterWithKey (\k _ -> i == (snd $ k)) targetWorld) < 10)) [0..17]
                      --indicesOfRemainingLines :: [Int]
                      --indicesOfRemainingLines = map fst $ filter snd $ lineRemaining
                      (indicesOfRemainingLines, indicesOfCompleteLines) =
                        let (r,c) = partition snd lineRemaining
                        in (map fst r, map fst c)
                      newWorldUnshifted :: Tetromino t => World t -> World t
                      newWorldUnshifted arg = Map.filterWithKey (\k _ -> (snd k) `elem` indicesOfRemainingLines) arg

                      newWorld :: Tetromino t => World t -> World t
                      newWorld arg = 
                          foldl clearLine arg $ sort indicesOfCompleteLines
                      clearLine :: Tetromino t => World t -> Int -> World t
                      clearLine w i = Map.mapKeys (\(x,y) -> if y >= i then (x,y-1) else (x,y)) w
          insertToWorld :: World t -> (t, (Coord, Coord, Coord, Coord)) -> World t
          insertToWorld targetWorld (blockType, (c1,c2,c3,c4)) = 
              foldr (\k m -> Map.insert k blockType m) targetWorld [c1,c2,c3,c4]
          newRandomBlock :: Tetromino t => t
          newRandomBlock = randomTetromino rnd
          nextBlockPos :: (Coord, Coord, Coord, Coord)
          nextBlockPos = pushToTop $ coords nb
          newLevels :: [Level]
          newLevels = if l - completeLines > 0
                      then (Level (l - completeLines) f):ls
                      else case ls of
                            []    -> [Level 0 f]
                            ((Level nl nf):t) -> (Level (nl + l - completeLines) nf):t 
placeAndNew other = other

--sort :: Ord a => [a] -> [a]
--sort [] = []
--sort (h:t) = (sort $ filter (<h) t) ++ (h:(sort $ filter (>=h) t))