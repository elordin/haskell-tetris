module KeyboardHandler where

import Control.Concurrent.STM
import Graphics.UI.GLUT
import System.Exit
import Control.Monad

data Input = OneDown | Spin | Drop | Hold | ShiftLeft | ShiftRight | Esc
    deriving(Show, Eq, Enum)

keyboardHandler :: TVar [Input] -> TVar Bool -> KeyboardMouseCallback
keyboardHandler _ _ (SpecialKey KeyF4) Down (Modifiers _ _ Down) _ = exitSuccess -- Alt+F4
keyboardHandler _ _ (SpecialKey KeyF11) Down _ _ = fullScreenToggle -- F11 - toggle fullscreen
keyboardHandler queue paused key Down _ _ = atomically $ do
    case key of
        (Char       '\r'     ) -> modifyTVar paused not
        (Char       '\n'     ) -> modifyTVar paused not
        (Char       'p'      ) -> modifyTVar paused not
        (Char       '\ESC'   ) -> modifyTVar queue (\q -> Esc:q)
        (SpecialKey KeyLeft  ) -> modifyTVar queue (\q -> ShiftLeft:q)
        (SpecialKey KeyRight ) -> modifyTVar queue (\q -> ShiftRight:q)
        (SpecialKey KeyUp    ) -> modifyTVar queue (\q -> Spin:q)
        (SpecialKey KeyDown  ) -> modifyTVar queue (\q -> OneDown:q)
        (SpecialKey KeyShiftR) -> modifyTVar queue (\q -> Hold:q)
        (Char       ' '      ) -> modifyTVar queue (\q -> Drop:q)
        _                      -> return ()
keyboardHandler _ _ _ _ _ _        =  return ()
