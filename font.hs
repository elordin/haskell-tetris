module Font where

import Graphics.UI.GLUT
import Data.Char (isUpper, toLower)

letterPath :: Char -> [(GLfloat, GLfloat, GLfloat)]
letterPath ' ' = []
--letterPath 'a'
--letterPath 'b'
--letterPath 'c'
--letterPath 'd'
letterPath 'e' = [ (1   , 0   , 0)
                 , (1   , 0.16, 0)
                 , (0   , 0   , 0)
                 , (0.33, 0.16, 0)
                  , (0   , 0.16, 0)
                 , (0.33, 0.5 , 0)
                 , (0   , 0.5 , 0)
                 , (0.83, 0.5 , 0)
                 , (0   , 0.66, 0)
                 , (0.83, 0.66, 0)
                 , (0.33, 0.66, 0)
                 , (0   , 0.66, 0)
                 , (0.33, 0.83, 0)
                 , (0   , 1   , 0)
                 , (1   , 0.83, 0)
                 , (1   , 1   , 0) ]
--letterPath 'f'
--letterPath 'g'
letterPath 'h' = [ (0   , 1   , 0)
                 , (0   , 0   , 0)
                 , (0.33, 1   , 0)
                 , (0.33, 0   , 0)
                 , (0.33, 0.4 , 0)
                 , (0.33, 0.6 , 0)
                 , (0.66, 0.6 , 0)
                 , (0.66, 0.4 , 0)
                 , (0.66, 1   , 0)
                 , (0.66, 0   , 0)
                 , (1   , 1   , 0)
                 , (1   , 0   , 0)]
letterPath 'i' = [ (0.16, 1   , 0)
                 , (0.83, 1   , 0)
                 , (0.16, 0.83, 0)
                 , (0.83, 0.83, 0)
                 , (0.33, 0.83, 0)
                 , (0.66, 0.83, 0)
                 , (0.33, 0.16, 0)
                 , (0.66, 0.16, 0)
                 , (0.16, 0.16, 0)
                 , (0.16, 0   , 0)
                 , (0.83, 0.16, 0)
                 , (0.83, 0   , 0) ]
--letterPath 'j'
--letterPath 'k'
--letterPath 'l'
--letterPath 'm'
--letterPath 'n'
letterPath 'o' = [ (1   , 2/7 , 0)
                 , (5/7 , 2/7 , 0)
                 , (1   , 5/7 , 0)
                 , (5/7 , 5/7 , 0)
                 , (5/7 , 1   , 0)
                 , (4/7 , 6/7 , 0)
                 , (0.5 , 1   , 0)
                 , (3/7 , 6/7 , 0)
                 , (2/7 , 1   , 0)
                 , (2/7 , 5/7 , 0)
                 , (0   , 5/7 , 0)
                 , (2/7 , 2/7 , 0)
                 , (0   , 2/7 , 0)
                 , (3/7 , 1/7 , 0)
                 , (2/7 , 0   , 0)
                 , (5/7 , 0   , 0)
                 , (4/7 , 1/7 , 0)
                 , (1   , 2/7 , 0)
                 , (5/7 , 2/7 , 0) ]
--letterPath 'p'
letterPath 'q' = (letterPath 'o') ++ [ (4/7 , 1/7 , 0)
                                     , (4/7 , 3/7 , 0)
                                     , (3/7 , 2/7 , 0) ]
--letterPath 'r'
--letterPath 's'
letterPath 't' = [ (0   , 1   , 0)
                 , (1   , 1   , 0)
                 , (0   , 0.83, 0)
                 , (1   , 0.83, 0)
                 , (0.33, 0.83, 0)
                 , (0.66, 0.83, 0)
                 , (0.33, 0   , 0)
                 , (0.66, 0   , 0) ]
letterPath 'u' = [ (1   , 1   , 0)
                 , (5/7 , 1   , 0)
                 , (1   , 2/7 , 0)
                 , (5/7 , 2/7 , 0)
                 , (5/7 , 0   , 0)
                 , (4/7 , 1/7 , 0)
                 , (0.5 , 0   , 0)
                 , (3/7 , 1/7 , 0)
                 , (2/7 , 0   , 0)
                 , (2/7 , 2/7 , 0)
                 , (0   , 2/7 , 0)
                 , (2/7 , 1   , 0)
                 , (0   , 1   , 0) ]
--letterPath 'v'
letterPath 'w' = [ (0, 1, 0)
                 , (2/8, 1, 0)
                 , (1/8, 0, 0)
                 , (3/8, 2/8, 0)
                 , (3/8, 0, 0)
                 , (4/8, 4/8, 0)
                 , (4/8, 2/8, 0)
                 , (5/8, 2/8, 0)
                 , (5/8, 0, 0)
                 , (6/8, 1, 0)
                 , (7/8, 0, 0)
                 --, (6/8, 1, 0)
                 , (1, 1, 0) ]
--letterPath 'x'
--letterPath 'y'
--letterPath 'z'
--letterPath '1'
--letterPath '2'
--letterPath '3'
--letterPath '4'
--letterPath '5'
--letterPath '6'
--letterPath '7'
--letterPath '8'
--letterPath '9'
letterPath '0' = letterPath 'o'

letterPath c   = if isUpper c
                 then letterPath $ toLower c
                 else [ (0, 1, 0) , (0, 0, 0) , (1, 1, 0) , (1, 0, 0) ]
