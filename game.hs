module Game where

import qualified Data.Map.Strict as Map

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
shiftBlock game@(Game w ls s p h n (blockType, ((x1,y1),(x2,y2),(x3,y3),(x4,y4))) rg) opX opY
    | not p && freeForBlock w newPos = Game w ls s p h n (blockType, newPos) rg
    | otherwise = game
    where newPos = ((opX x1, opY y1),(opX x2, opY y2),(opX x3, opY y3),(opX x4, opY y4))
shiftBlock other _ _ = other

freeForBlock :: Tetromino t =>  World t -> (Coord, Coord, Coord, Coord) -> Bool
freeForBlock w (a,b,c,d) =
    and $ map (\(x,y) -> x >= 0
        && x < blocksX
        && y >= 0
        && y < blocksY
        && case Map.lookup (x,y) w of
            Nothing   -> True
            Just _    -> False) [a,b,c,d]

spin :: Game -> Game
spin game@(Game w ls s p h n (blockType, coords@(cor, _, _, _)) rg)
    | (not p) && (freeForBlock w rotated) = Game w ls s p h n (blockType, rotated) rg
    | otherwise = game
    where rotated = rotateNormalized coords cor Clockwise
spin other = other

dropDown :: Game -> Game
dropDown = (foldl (\f _ -> shiftDown.f) shiftDown [1..blocksY])

holdBlock :: Game -> Game
holdBlock (Game _ _ _ _ _ _ _ []) = GameError "Out of random numbers."
holdBlock game@(Game w ls s p (h,ch) n (a,_) (rnd:rnds))
    | p || not ch = game
    | otherwise = case h of
        Nothing -> let newPos = pushToTop $ coords n
                   in if freeForBlock w newPos
                      then Game w ls s p (Just a, False) (get rnd) (n, newPos) rnds
                      else GameOver s
        Just ho -> let newPos = pushToTop $ coords ho
                   in if freeForBlock w newPos
                      then Game w ls s p (Just a, False) n (ho, newPos) rnds
                      else GameOver s
holdBlock other = other

placeAndNew :: Game -> Game
placeAndNew (Game _ [] _ _ _ _ _ _) = GameError "Out of levels."
placeAndNew (Game _ _ _ _ _ _ _ []) = GameError "Out of random numbers."
placeAndNew (Game w ((Level l f):ls) s p (h,ch) nb ab (rnd:rnds))
    | freeForBlock w nextBlockPos = Game newWorld newLevels (s + (scorePerLines completeLines)) p (h,True) (get rnd) (nb, nextBlockPos) rnds
    | otherwise = GameOver s
    where
        nextBlockPos :: (Coord, Coord, Coord, Coord)
        nextBlockPos = pushToTop $ coords nb
        (newWorld, completeLines) = clearCompleteLines $ insertIntoWorld w ab
        clearCompleteLines :: Tetromino t => World t -> (World t, Int)
        clearCompleteLines world = foldr (\i (w,n) -> if (blocksInLine w i) > 9 then (removeLine w i, n+1) else (w, n)) (world, 0) [0..blocksY]
        blocksInLine :: Tetromino t => World t -> Int -> Int
        blocksInLine world line = Map.size $ Map.filterWithKey (\(_,y) _ -> y == line) world
        removeLine :: Tetromino t => World t -> Int -> World t
        removeLine world line = Map.mapKeys (\(x,y) -> if y > line then (x,y-1) else (x,y)) $ Map.filterWithKey (\(_,y) _ -> y /= line) world
        insertIntoWorld :: Tetromino t => World t -> (t, (Coord, Coord, Coord, Coord)) -> World t
        insertIntoWorld targetWorld (blockType, (c1,c2,c3,c4)) =
            foldr (\k m -> Map.insert k blockType m) targetWorld [c1,c2,c3,c4]
        newLevels :: [Level]
        newLevels
            | l - completeLines > 0 = (Level (l - completeLines) f):ls
            | otherwise = case ls of
                []    -> [Level 0 f]
                ((Level nl nf):t) -> (Level (nl + l - completeLines) nf):t
placeAndNew other = other

sort::Ord a=>[a]->[a]
sort l=case l of{[]->[];(h:t)->(sort$filter(<h)t)++(h:(sort$filter(>=h)t))}
