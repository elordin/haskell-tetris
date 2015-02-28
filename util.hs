{-# LANGUAGE GADTs #-}

module Util where

import Graphics.UI.GLUT hiding (Level)
import Data.Map.Strict as Map

-- 'Constants' definitions

-- Colours
white  = color $ Color3 (1 :: GLfloat) (1 :: GLfloat) (1 :: GLfloat)
black  = color $ Color3 (0 :: GLfloat) (0 :: GLfloat) (0 :: GLfloat)
lightG = color $ Color3 (0.6 :: GLfloat) (0.8 :: GLfloat) (0.6 :: GLfloat)
darkG  = color $ Color3 (0.2 :: GLfloat) (0.3 :: GLfloat) (0.2 :: GLfloat)

-- Data definitions

type Coord = (Int, Int)

-- Keyboard input
--type Input = OneDown | Spin | Drop | Hold | ShiftLeft | ShiftRight | Esc
    --deriving(Show, Eq, Enum)

-- Rotation
data Rotation = Clockwise | CounterClockwise

-- Blocks

class Tetromino a where
    -- colour scheme from the outside in
    colorScheme :: a -> (IO (), IO (), IO (), IO ())
    -- initial coordinates, centre of rotation should be first
    coords :: a -> Maybe (Coord, Coord, Coord, Coord)

    --rotate :: a -> Rotation -> (Coord, Coord, Coord, Coord)

data Block   = Tb | Ob | Ib | Jb | Sb | Lb | Zb | Void
    deriving(Eq)
instance Show Block where
    show Tb   = "T"
    show Ob   = "O"
    show Ib   = "I"
    show Jb   = "J"
    show Sb   = "S"
    show Lb   = "L"
    show Zb   = "Z"
    show Void = " "
instance Tetromino Block where
    colorScheme Zb   = (black, lightG, lightG, black )
    colorScheme Sb   = (black, darkG,  black,  white )
    colorScheme Ob   = (black, white,  black,  black )
    colorScheme Ib   = (black, lightG, lightG, lightG)
    colorScheme Jb   = (black, lightG, black,  white )
    colorScheme Lb   = (black, darkG,  darkG,  darkG )
    colorScheme Tb   = (black, lightG, black,  lightG)
    colorScheme Void = (white, white,  white,  white )

    coords Zb   = Just ((0, 0), (-1,  1), (0, 1), (1, 0))
    coords Sb   = Just ((0, 0), (-1,  0), (0, 1), (1, 1))
    coords Ob   = Just ((0, 0), ( 0,  1), (1, 0), (1, 1))
    coords Ib   = Just ((0, 0), ( 0, -1), (0, 1), (0, 2))
    coords Jb   = Just ((0, 0), (-1,  0), (0, 1), (1, 2))
    coords Lb   = Just ((0, 0), ( 1,  0), (0, 1), (1, 2))
    coords Tb   = Just ((0, 0), (-1,  0), (0, 1), (1, 0))
    coords Void = Nothing

type World = Map.Map (Int, Int) Block

data Level = Level {lines::Int, frequency::Int}
    deriving(Show)

data MItem = Start | Quit
    deriving(Show)

data Game where 
    -- GADT syntax required for type constraint
    --Game :: Tetromino t => { world       :: World
    --                       , level       :: Level
    --                       , levels      :: [Level]
    --                       , score       :: Int
    --                       , paused      :: Bool
    --                       , hold        :: Maybe Block 
    --                       , activeBlock :: (t, Int, Int) } -> Game
    Game :: { world       :: World
            , level       :: Level
            , levels      :: [Level]
            , score       :: Int
            , paused      :: Bool
            , hold        :: Maybe Block 
            , activeBlock :: (Block, Int, Int) } -> Game
    GameMenu :: MItem -> Game

defaultNewGame = 
    Game (Map.empty) (Level 1 500) [{-- level definitions --}] 0 False Nothing (Tb, 4, 10)


rotateNormalized :: (Coord, Coord, Coord, Coord) -> Coord -> Rotation -> (Coord, Coord, Coord, Coord)
--rotateNormalized :: Tetromino t => t -> Coord -> Rotation -> (Coord, Coord, Coord, Coord)
rotateNormalized (o1, o2, o3, o4) (cx, cy) r =
    case r of
        Clockwise        -> ((o1ny, -1 * o1nx), (o2ny, -1 * o2nx), (o3ny, -1 * o3nx), (o4ny, -1 * o4nx))
        CounterClockwise -> ((-1 * o1ny, o1nx), (-1 * o2ny, o2nx), (-1 * o3ny, o3nx), (-1 * o4ny, o4nx))
    where normalize (x,y) = (x - cx, y - cy)
          (o1nx, o1ny)    = normalize o1
          (o2nx, o2ny)    = normalize o2
          (o3nx, o3ny)    = normalize o3
          (o4nx, o4ny)    = normalize o4
