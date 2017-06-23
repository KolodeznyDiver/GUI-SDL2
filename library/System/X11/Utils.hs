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

module System.X11.Utils(
    hideConsole,withRemoveFromTaskbar
    ) where

import Control.Monad.IO.Class

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
