{-# LANGUAGE GADTs #-}

module Util where

import Graphics.UI.GLUT hiding (Level)
import qualified Data.Map.Strict as Map
import System.Random
import Data.Time.Clock

-- 'Constants' definitions
-- Colours
white  = color $ Color3 (1 :: GLfloat) (1 :: GLfloat) (1 :: GLfloat)
gray   = color $ Color3 (0.5 :: GLfloat) (0.5 :: GLfloat) (0.5 :: GLfloat)
black  = color $ Color3 (0 :: GLfloat) (0 :: GLfloat) (0 :: GLfloat)
lightG = color $ Color3 (0.6 :: GLfloat) (0.8 :: GLfloat) (0.6 :: GLfloat)
darkG  = color $ Color3 (0.2 :: GLfloat) (0.3 :: GLfloat) (0.2 :: GLfloat)

-- Data definitions
type Coord = (Int, Int)
data Rotation = Clockwise | CounterClockwise
class Tetromino a where
    -- colour scheme from the outside in
    colorScheme :: a -> (IO (), IO (), IO (), IO ())
    -- initial coordinates, centre of rotation should be first
    coords :: a -> (Coord, Coord, Coord, Coord)
    randomTetromino :: Int -> a


data Block   = Tb | Ob | Ib | Jb | Sb | Lb | Zb
    deriving(Eq, Enum)
instance Show Block where
    show Tb   = "T"
    show Ob   = "O"
    show Ib   = "I"
    show Jb   = "J"
    show Sb   = "S"
    show Lb   = "L"
    show Zb   = "Z"
instance Tetromino Block where
    randomTetromino i = toEnum $ mod i 6

    colorScheme Zb   = (black, lightG, lightG, black )
    colorScheme Sb   = (black, darkG,  black,  white )
    colorScheme Ob   = (black, white,  black,  black )
    colorScheme Ib   = (black, lightG, lightG, lightG)
    colorScheme Jb   = (black, lightG, black,  white )
    colorScheme Lb   = (black, darkG,  darkG,  darkG )
    colorScheme Tb   = (black, lightG, black,  lightG)

    coords Zb   = ((0, 0), (-1,  1), ( 0, 1), (1, 0))
    coords Sb   = ((0, 0), (-1,  0), ( 0, 1), (1, 1))
    coords Ob   = ((0, 0), ( 0,  1), ( 1, 0), (1, 1))
    coords Ib   = ((0, 0), ( -1, 0), ( 1, 0), (2, 0))
    coords Jb   = ((0, 0), (-1,  1), (-1, 0), (1, 0))
    coords Lb   = ((0, 0), (-1,  0), ( 1, 0), (1, 1))
    coords Tb   = ((0, 0), (-1,  0), ( 0, 1), (1, 0))

type World t = Map.Map (Int, Int) t

data Level = Level {lines::Int, frequency::Int}
    deriving(Show)

data MItem = Start | Quit
    deriving(Show)

data Game where
    Game :: Tetromino t => { world       :: World t
                           , levels      :: [Level]
                           , score       :: Int
                           , paused      :: Bool
                           , hold        :: (Maybe t, Bool)
                           , nextBlock   :: t
                           , activeBlock :: (t, (Coord, Coord, Coord, Coord)) 
                           , rnds        :: [Int] } -> Game
    GameMenu :: MItem -> Game
    --GameOver :: Game

defaultNewGame :: Int -> Game
defaultNewGame seed = 
    Game Map.empty
         [(Level 10 500)] 
         0
         False 
         (Just Ob, True)
         Lb
         (firstActiveBlock, pushToTop $ coords firstActiveBlock)
         rnds
    where i:rnds = randomRs (0,6) $ mkStdGen seed
          firstActiveBlock :: Block
          firstActiveBlock = randomTetromino i 

pushToTop :: (Coord, Coord, Coord, Coord) -> (Coord, Coord, Coord, Coord)
pushToTop (c1,c2,c3,c4) = let push (x,y) = (x+4, y+16)
                          in (push c1, push c2, push c3, push c4)

rotateNormalized :: (Coord, Coord, Coord, Coord) -> Coord -> Rotation -> (Coord, Coord, Coord, Coord)
rotateNormalized (o1, o2, o3, o4) (cx, cy) r =
    case r of
        Clockwise        -> ( (o1ny + cx, -1 * o1nx + cy)
                            , (o2ny + cx, -1 * o2nx + cy)
                            , (o3ny + cx, -1 * o3nx + cy)
                            , (o4ny + cx, -1 * o4nx + cy) )
        CounterClockwise -> ( (-1 * o1ny + cx, o1nx + cy)
                            , (-1 * o2ny + cx, o2nx + cy)
                            , (-1 * o3ny + cx, o3nx + cy)
                            , (-1 * o4ny + cx, o4nx + cy) )
    where normalize (x,y) = (x - cx, y - cy)
          (o1nx, o1ny)    = normalize o1
          (o2nx, o2ny)    = normalize o2
          (o3nx, o3ny)    = normalize o3
          (o4nx, o4ny)    = normalize o4

scorePerLines :: Int -> Int
scorePerLines 4 = 1000
scorePerLines 3 =  500
scorePerLines 2 =  250
scorePerLines 1 =  100
scorePerLines _ =    0
