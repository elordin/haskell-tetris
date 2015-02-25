module Util where

import Graphics.UI.GLUT

-- 'Constants' definitions

-- Colours
white  = color $ Color3 (1 :: GLfloat) (1 :: GLfloat) (1 :: GLfloat)
black  = color $ Color3 (0 :: GLfloat) (0 :: GLfloat) (0 :: GLfloat)
lightG = color $ Color3 (0.6 :: GLfloat) (0.8 :: GLfloat) (0.6 :: GLfloat)
darkG  = color $ Color3 (0.2 :: GLfloat) (0.3 :: GLfloat) (0.2 :: GLfloat)

-- Data definitions

-- Blocks
class Tetromino a where
    -- colour scheme from the outside in
    colorScheme :: a -> (IO (), IO (), IO (), IO ())
    -- initial coordinates, centre of rotation should be first
    coords :: a -> Maybe ((Int,Int), (Int,Int), (Int,Int), (Int,Int))

data Block   = Tb | Ob | Ib | Jb | Sb | Lb | Zb | Void
    deriving(Show, Eq)
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

data World   = World [[Block]]
    deriving(Show)
data Game = Game {world::World}

-- rotate :: ((Int,Int), (Int,Int), (Int,Int), (Int,Int)) -> (Int, Int) -> ((Int,Int), (Int,Int), (Int,Int), (Int,Int))
