module KeyboardHandler where

import Control.Concurrent.STM
import Graphics.UI.GLUT hiding (Level)
import System.Exit
import Control.Monad
import Util

keyboardHandler :: TVar [Key] -> TVar Bool -> KeyboardMouseCallback
keyboardHandler _ _ (SpecialKey KeyF4) Down (Modifiers _ _ Down) _ = exitSuccess -- Alt+F4
keyboardHandler _ _ (SpecialKey KeyF11) Down _ _ = fullScreenToggle -- F11 - toggle fullscreen
keyboardHandler queue paused key Down _ _ = atomically $ do
    case key of
        (Char       '\r'     ) -> modifyTVar queue (\q -> (Char '\r'):q)
        (Char       '\n'     ) -> modifyTVar queue (\q -> (Char '\n'):q)
        (Char       'p'      ) -> do modifyTVar paused not; modifyTVar queue (\q -> (Char 'p'):q)
        (Char       '\ESC'   ) -> modifyTVar queue (\q -> (Char '\ESC'):q)
        (SpecialKey KeyLeft  ) -> modifyTVar queue (\q -> (SpecialKey KeyLeft):q)
        (SpecialKey KeyRight ) -> modifyTVar queue (\q -> (SpecialKey KeyRight):q)
        (SpecialKey KeyUp    ) -> modifyTVar queue (\q -> (SpecialKey KeyUp):q)
        (SpecialKey KeyDown  ) -> modifyTVar queue (\q -> (SpecialKey KeyDown):q)
        (SpecialKey KeyShiftR) -> modifyTVar queue (\q -> (SpecialKey KeyShiftR):q)
        (Char       ' '      ) -> modifyTVar queue (\q -> (Char ' '):q)
        _                      -> return ()
keyboardHandler _ _ _ _ _ _    =  return ()
