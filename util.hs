{-# LANGUAGE GADTs #-}

module Util where

import Graphics.UI.GLUT hiding (Level)
import qualified Data.Map.Strict as Map
import System.Random

-- 'Constants' definitions
-- Colours
white  = color $ Color3 (1 :: GLfloat) (1 :: GLfloat) (1 :: GLfloat)
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
    coords :: a -> Maybe (Coord, Coord, Coord, Coord)
    --rotate :: a -> Rotation -> (Coord, Coord, Coord, Coord)

data Block   = Tb | Ob | Ib | Jb | Sb | Lb | Zb | Void
    deriving(Eq, Enum)
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

type World t = Map.Map (Int, Int) t

data Level = Level {lines::Int, frequency::Int}
    deriving(Show)

data MItem = Start | Quit
    deriving(Show)

data Game where
    Game :: Tetromino t => { world       :: World t
                           , level       :: Level
                           , levels      :: [Level]
                           , score       :: Int
                           , paused      :: Bool
                           , hold        :: Maybe t
                           , activeBlock :: (t, (Coord, Coord, Coord, Coord)) } -> Game
    GameMenu :: MItem -> Game

defaultNewGame = 
    Game (fst $ foldr (\k (m,rnd) -> 
            let c = randomR (0, 100) rnd 
            in if (fst c :: Int) > 50 
               then (Map.insert k Tb m, snd c) 
               else (m, snd c)) (Map.empty, mkStdGen 10) $ concatMap (\n -> map (\m -> (n,m)) [0..9]) [0..5]) 
         (Level 1 500) 
         [{-- level definitions --}] 
         0 
         False 
         (Just Ob)
         ((\rnd -> let n = fst $ randomR (0, 6) rnd
                       b :: Block
                       b = toEnum n 
                       c = case coords b of
                            Nothing -> ((0,0), (0,0), (0,0), (0,0))
                            Just ((x1,y1), (x2,y2), (x3, y3), (x4,y4)) -> ( (x1+4,y1+12)
                                                                          , (x2+4,y2+12)
                                                                          , (x3+4,y3+12)
                                                                          , (x4+4,y4+12) )
                   in (b, c)) $ mkStdGen 10)

rotateNormalized :: (Coord, Coord, Coord, Coord) -> Coord -> Rotation -> (Coord, Coord, Coord, Coord)
--rotateNormalized :: Tetromino t => t -> Coord -> Rotation -> (Coord, Coord, Coord, Coord)
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
