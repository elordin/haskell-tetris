{-# LANGUAGE GADTs #-}

module Util where

import Graphics.UI.GLUT (GLsizei, GLfloat, color, Vertex3(..), vertex, Color3(..))
import qualified Data.Map.Strict as Map
import System.Random (mkStdGen, randomRs)

windowWidth  :: GLsizei
windowWidth  = 720
windowHeight :: GLsizei
windowHeight = 720

blocksX :: Int
blocksX = 10
blocksY :: Int
blocksY = 18

setRGBColor :: GLfloat -> GLfloat -> GLfloat -> IO ()
setRGBColor r g b = color $ Color3 r g b

white  = setRGBColor 1 1 1
gray   = setRGBColor 0.5 0.5 0.5
black  = setRGBColor 0 0 0
lightG = setRGBColor 0.6 0.8 0.6
darkG  = setRGBColor 0.2 0.3 0.2
red    = setRGBColor 0.8 0 0

type Coord = (Int, Int)

data Rotation = Clockwise | CounterClockwise

class Tetromino a where
    -- defined colour-scheme of a Tetromino (ouside-in)
    colorScheme :: a -> (IO (), IO (), IO (), IO ())
    -- coordinates of the four blocks relative to an arbitrary centre
    coords      :: a -> (Coord, Coord, Coord, Coord)
    -- returns a specific Tetromino
    get         :: Int -> a

data Block   = Tb | Ob | Ib | Jb | Sb | Lb | Zb
    deriving(Eq, Enum)
instance Tetromino Block where
    get i = toEnum $ mod i 7

    colorScheme Zb = (black, lightG, lightG, black )
    colorScheme Sb = (black, darkG,  black,  white )
    colorScheme Ob = (black, white,  black,  black )
    colorScheme Ib = (black, lightG, lightG, lightG)
    colorScheme Jb = (black, lightG, black,  white )
    colorScheme Lb = (black, darkG,  darkG,  darkG )
    colorScheme Tb = (black, lightG, black,  lightG)

    coords Zb = ((0, 0), (-1,  1), ( 0, 1), (1, 0))
    coords Sb = ((0, 0), (-1,  0), ( 0, 1), (1, 1))
    coords Ob = ((0, 0), ( 0,  1), ( 1, 0), (1, 1))
    coords Ib = ((0, 0), ( -1, 0), ( 1, 0), (2, 0))
    coords Jb = ((0, 0), (-1,  1), (-1, 0), (1, 0))
    coords Lb = ((0, 0), (-1,  0), ( 1, 0), (1, 1))
    coords Tb = ((0, 0), (-1,  0), ( 0, 1), (1, 0))

type World t = Map.Map (Int, Int) t

data Level = Level {lines::Int, frequency::Int}
    deriving(Show)

data MenuItem = Start | Quit
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
    GameMenu  :: MenuItem -> Game
    GameOver  :: Int -> Game
    GameError :: String -> Game

defaultNewGame :: Int -> Game
defaultNewGame seed =
    Game Map.empty
         [Level 10 700, Level 10 600, Level 10 500, Level 15 400, Level 25 300, Level 30 200]
         0
         False
         (Nothing, True)
         Lb
         (firstActiveBlock, pushToTop $ coords firstActiveBlock)
         rnds
    where i:rnds = randomRs (0,6) $ mkStdGen seed
          firstActiveBlock :: Block
          firstActiveBlock = get i

-- pushes a Tetromino to the top of the playing board
pushToTop :: (Coord, Coord, Coord, Coord) -> (Coord, Coord, Coord, Coord)
pushToTop (c1,c2,c3,c4) = let push (x,y) = (x + blocksX `div` 2,y + blocksY - 2)
                          in (push c1, push c2, push c3, push c4)

-- rotates around a given centre of rotation
rotateNormalized :: (Coord, Coord, Coord, Coord) -> Coord -> Rotation -> (Coord, Coord, Coord, Coord)
rotateNormalized (o1, o2, o3, o4) (cx, cy) r =
    case r of
        Clockwise        -> ( (o1ny + cx, -1 * o1nx + cy) , (o2ny + cx, -1 * o2nx + cy)
                            , (o3ny + cx, -1 * o3nx + cy) , (o4ny + cx, -1 * o4nx + cy) )
        CounterClockwise -> ( (-1 * o1ny + cx, o1nx + cy) , (-1 * o2ny + cx, o2nx + cy)
                            , (-1 * o3ny + cx, o3nx + cy) , (-1 * o4ny + cx, o4nx + cy) )
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

-- utility function for creating vertices
v3 :: (GLfloat, GLfloat, GLfloat) -> IO ()
v3 (x,y,z) = vertex $ Vertex3 x y z

-- utility function for creating multiple vertices at once
mapV3 :: [(GLfloat, GLfloat, GLfloat)] -> IO ()
mapV3 = mapM_ v3
