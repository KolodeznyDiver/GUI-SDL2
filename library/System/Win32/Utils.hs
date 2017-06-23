{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
-- |
-- Module:      System.Win32.Utils
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <kolodeznydiver@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Вспомогательные функции, которые выполняют действия, зависящие от операционной системы.
--
-- Этот модуль используется при компиляции под ОС MS Windows,
-- для *nix систем см. "System.X11.Utils".

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

-- | Функция прячет консольное окно на экране.
-- Удалять консольное окно или собирать Haskell приложение как оконное, а не как консольное,
-- нежелательно под Windows, т.к. в этом случае любая попытка вывода в 'Stdout', 'Stderr'
-- приводит к 'Exception'.
-- См. <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/win32-dlls.html#interacting-with-the-terminal>.
hideConsole :: IO ()
hideConsole = do
    wConsole <- getConsoleWindow
    when (wConsole /= nullPtr) $ do
        void $ showWindow wConsole sW_HIDE
        es <- getWindowLong wConsole GWL_EXSTYLE
        void $ c_SetWindowLongPtr wConsole GWL_EXSTYLE $ wordPtrToPtr $ fromIntegral
            (wS_EX_TOOLWINDOW .|. fromIntegral es)

-- Copypast from "Graphics.Win32.Window".
findWindowByName :: String -> IO (Maybe HWND)
findWindowByName wname = withTString wname $ \ c_wname ->
  liftM ptrToMaybe $ c_FindWindow nullPtr c_wname

foreign import WINDOWS_CCONV unsafe "Windows.h GetActiveWindow"
    getActiveWindow :: IO HWND

-- | Функция удаляет создаваемое в переданной ей функии окно из пенели задач.
-- Создаваемые библиотекой SDL окна по умолчанию оказываются видны в панели задач,
-- но приложению желательно иметь только одно окно в панели задач.
-- Т.к. мне неизвестен способ получить HANDLE окна MS Windows от SDL, HANDLE находится по заголовку окна
-- которое должно быть уникальное среди других окон, не имеющих родительского.
-- @withRemoveFromTaskbar@ запоминает активное окно на момент его вызова, затем выполняет переданную ей
-- функцию, находит созданное окно по заголовку и делает его дочерним окном предыдущего.
withRemoveFromTaskbar :: MonadIO m =>
    String -> -- ^ Заголовок, который будет установлен у создаваемого окна. Должен быть уникальным.
    m a -> -- ^ Функция создающее окно.
    m a
withRemoveFromTaskbar title f = do
    wParent <- liftIO getActiveWindow
    r <- f
    liftIO $ do
        mbW <- findWindowByName title
        whenIsJust mbW $ \ w ->
            void $ c_SetWindowLongPtr w GWLP_HWNDPARENT $ castPtr wParent
    return r








