module KeyboardHandler where

import Control.Concurrent.STM (TVar, atomically, modifyTVar)
import Graphics.UI.GLUT (KeyboardMouseCallback, Key(..), SpecialKey(..), KeyState(..), Modifiers(..), Position(..), fullScreenToggle)
import System.Exit (exitSuccess)

keyboardHandler :: TVar [Key] -> KeyboardMouseCallback
keyboardHandler _ (SpecialKey KeyF4) Down (Modifiers _ _ Down) _ = exitSuccess
keyboardHandler _ (SpecialKey KeyF11) Down _ _ = fullScreenToggle
keyboardHandler queue key Down _ _
    | key `elem` validKeys = addToQueue
    | otherwise = return ()
    where validKeys  = [ Char '\r'
                       , Char '\n'
                       , Char '\ESC'
                       , Char ' '
                       , SpecialKey KeyLeft
                       , SpecialKey KeyRight
                       , SpecialKey KeyUp
                       , SpecialKey KeyDown
                       , SpecialKey KeyShiftR ]
          addToQueue = atomically $ modifyTVar queue (\q -> key:q)
keyboardHandler _ _ _ _ _ = return ()
