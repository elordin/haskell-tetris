module KeyboardHandler where

import Control.Concurrent.STM (TVar, atomically, modifyTVar)
import Graphics.UI.GLUT (KeyboardMouseCallback, Key(..), SpecialKey(..), KeyState(..), Modifiers(..), Position(..), fullScreenToggle)
import System.Exit (exitSuccess)
--import Util

keyboardHandler :: TVar [Key] -> TVar Bool -> KeyboardMouseCallback
keyboardHandler _ _ (SpecialKey KeyF4) Down (Modifiers _ _ Down) _ = exitSuccess -- Alt+F4
keyboardHandler _ _ (SpecialKey KeyF11) Down _ _ = fullScreenToggle -- F11 - toggle fullscreen
keyboardHandler queue paused key Down _ _ =
    if key `elem` validKeys then addToQueue else return ()
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
keyboardHandler _ _ _ _ _ _    =  return ()
