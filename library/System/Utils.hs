{-# LANGUAGE CPP #-}
-- |
-- Module:      System.Utils
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <kolodeznydiver@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Вспомогательные функции, которые выполняют действия, зависящие от операционной системы.
--
-- Для MS Windows см. "System.Win32.Utils", для *nix систем см. "System.X11.Utils".

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
