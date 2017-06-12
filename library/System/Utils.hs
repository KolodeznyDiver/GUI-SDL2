{-# LANGUAGE CPP #-}
module System.Utils(
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
    module System.Win32.Utils
#else
    module System.X11.Utils
#endif
    ) where

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import System.Win32.Utils
#else
import System.X11.Utils
#endif
