{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
module System.Win32.Utils(
    hideConsole,withRemoveFromTaskbar
    ) where

import Data.Bits
import Foreign hiding(void)
import Control.Monad
import Control.Monad.IO.Class
import System.Win32
import Graphics.Win32.GDI.Types
import Graphics.Win32.Window
import Maybes (whenIsJust)

#if defined(i386_HOST_ARCH)
#   define WINDOWS_CCONV stdcall
#elif defined(x86_64_HOST_ARCH)
#   define WINDOWS_CCONV ccall
#else
#   error Unknown mingw32 arch
#endif

-- pattern GWL_STYLE :: INT; pattern GWL_STYLE = -16
pattern GWL_EXSTYLE :: INT; pattern GWL_EXSTYLE = -20
pattern GWLP_HWNDPARENT :: INT; pattern GWLP_HWNDPARENT = -8

foreign import WINDOWS_CCONV unsafe "Wincon.h GetConsoleWindow"
    getConsoleWindow :: IO HWND

foreign import WINDOWS_CCONV unsafe "Windows.h GetWindowLongA"
    getWindowLong :: HWND -> INT -> IO LONG

hideConsole :: IO ()
hideConsole = do
    wConsole <- getConsoleWindow
    void $ showWindow wConsole sW_HIDE
    es <- getWindowLong wConsole GWL_EXSTYLE
    void $ c_SetWindowLongPtr wConsole GWL_EXSTYLE $ wordPtrToPtr $ fromIntegral
        (wS_EX_TOOLWINDOW .|. fromIntegral es)

-- copypast from Graphics.Win32.Window
findWindowByName :: String -> IO (Maybe HWND)
findWindowByName wname = withTString wname $ \ c_wname ->
  liftM ptrToMaybe $ c_FindWindow nullPtr c_wname

foreign import WINDOWS_CCONV unsafe "Windows.h GetActiveWindow"
    getActiveWindow :: IO HWND

withRemoveFromTaskbar :: MonadIO m => String -> m a -> m a
withRemoveFromTaskbar title f = do
    wParent <- liftIO getActiveWindow
    r <- f
    liftIO $ do
        mbW <- findWindowByName title
        whenIsJust mbW $ \ w ->
            void $ c_SetWindowLongPtr w GWLP_HWNDPARENT $ castPtr wParent
    return r








