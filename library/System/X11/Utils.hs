{-# LANGUAGE CPP #-}
-- |
-- Module:      System.X11.Utils
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Вспомогательные функции, которые выполняют действия, зависящие от операционной системы.
--
-- Этот модуль используется при компиляции под *nix системами,
-- для ОС MS Windows см. "System.Win32.Utils".

module System.X11.Utils
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
    (
    hideConsole,withRemoveFromTaskbar,getWinBorders,getUILang
    )
#endif
    where

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)

import Data.Maybe
import Control.Monad.IO.Class
import System.Locale.SetLocale
import Control.Exception.Safe
import SDL.Vect
import Foreign.C.Types
import Graphics.X11
import Graphics.X11.Xlib.Extras

-- | Под MS Windows функция прячет консольное окно на экране, см. @System.Win32.Utils.hideConsole@.
-- В настоящий момент @hideConsole@ из этого модуля ничего не делает.
hideConsole:: IO ()
hideConsole = return ()

-- | Под MS Windows функция удаляет создаваемое в переданной ей функии окно из пенели задач MS Windows.
-- См. @System.Win32.Utils.withRemoveFromTaskbar@.
-- В настоящий момент @withRemoveFromTaskbar@ из этого модуля ничего не делает.
withRemoveFromTaskbar :: MonadIO m =>
    String -> -- ^ Заголовок, который будет установлен у создаваемого окна. Должен быть уникальным.
    m a -> -- ^ Функция создающее окно.
    m a
withRemoveFromTaskbar _title f = f

-- | Функция возвращает поля от видимой границы окна до клиентской области.
getWinBorders :: MonadIO m => m (V4 CInt)
getWinBorders = liftIO $ bracket (openDisplay "") closeDisplay $ \display -> do
--      liftIO $ putStrLn $ concat ["getWinBorders  display=", show display]
      a <- internAtom display "_NET_FRAME_EXTENTS" 
                True  --  only_if_exists
--      liftIO $ putStrLn $ concat ["getWinBorders  a=", show a]
      (win,_) <- getInputFocus display 
--      liftIO $ putStrLn $ concat ["getWinBorders  win=", show win]
      mbP <- getWindowProperty32 display a win
--      liftIO $ putStrLn $ concat ["getWinBorders  mbP=", show mbP]
      case mbP of
        Just [l,r,t,b] -> return $ V4 (fromIntegral l) (fromIntegral r) (fromIntegral t) (fromIntegral b)
        _ -> return $ V4 0 0 0 0

-- | Должна возвращать, например "en_US" or "ru_Ru" (m.b. "ru_Ru.*") или выбрасывать исключение.
getUILang :: IO String
getUILang = fromJust <$> (setLocale LC_ALL (Just "") >> setLocale LC_ALL Nothing)


-- https://stackoverflow.com/questions/12170488/how-to-get-current-locale-of-my-environment/12170538#12170538
{-
--#include <locale.h>
using namespace std;
setlocale(LC_ALL, "");
    cout << "LC_ALL: " << setlocale(LC_ALL, NULL) << endl;
$ ./a.out 
LC_ALL: en_US.utf8


-}
#else
data SystemUtilsUnused
#endif
