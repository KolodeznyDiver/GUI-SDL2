{-# LANGUAGE CPP #-}
-- |
-- Module:      System.X11.Utils
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <kolodeznydiver@gmail.com>
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
    hideConsole,withRemoveFromTaskbar,getUILang
    )
#endif
    where

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
--import Data.Maybe
import Control.Monad.IO.Class
import System.Locale.SetLocale

-- | Под MS Windows функция прячет консольное окно на экране, см. @System.Win32.Utils.hideConsole@.
-- В настоящий момент @hideConsole@ из этого модуля ничего не делает.
-- GUI-SDL2 не тестировалась под *nix.
hideConsole:: IO ()
hideConsole = return ()

-- | Под MS Windows функция удаляет создаваемое в переданной ей функии окно из пенели задач MS Windows.
-- См. @System.Win32.Utils.withRemoveFromTaskbar@.
-- В настоящий момент @withRemoveFromTaskbar@ из этого модуля ничего не делает.
-- GUI-SDL2 не тестировалась под *nix.
withRemoveFromTaskbar :: MonadIO m =>
    String -> -- ^ Заголовок, который будет установлен у создаваемого окна. Должен быть уникальным.
    m a -> -- ^ Функция создающее окно.
    m a
withRemoveFromTaskbar _title f = f

-- | Должна возвращать, например "en_US" or "ru_Ru" (m.b. "ru_Ru.*") или выбрасывать исключение.
getUILang :: IO String
getUILang = setLocale LC_ALL (Just "") >> setLocale LC_ALL Nothing >>= fmap fromJust


-- https://stackoverflow.com/questions/12170488/how-to-get-current-locale-of-my-environment/12170538#12170538
{-
#include <locale.h>
using namespace std;
setlocale(LC_ALL, "");
    cout << "LC_ALL: " << setlocale(LC_ALL, NULL) << endl;
$ ./a.out 
LC_ALL: en_US.utf8


-}
#else
data SystemUtilsUnused
#endif
