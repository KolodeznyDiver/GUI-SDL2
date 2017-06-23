{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:      GUI.BaseLayer.Depend0.Auxiliaries
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <kolodeznydiver@gmail.com>
-- Stability:   experimental
-- Portability: portable
--

module GUI.BaseLayer.Depend0.Auxiliaries(
    -- * Некоторые вспомогательные функции.
    getAppName,mkRandomWinTitle,showErrMsgBoxT,showErrMsgBoxTL,showErrMsgBoxB,showErrMsgBox,guiCatch
    ) where

import System.Environment (getProgName)
import Control.Exception
import System.Exit
import System.CPUTime
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified TextShow as TS
import System.FilePath
import qualified SDL

-- | Возвращает имя текущего исполняемого файла без расширения и пути.
getAppName :: IO String
getAppName = takeBaseName <$> getProgName
{-# INLINE getAppName #-}

-- | Возвращает строку, годную для временного, предположительно уникального, заголовка окна.
mkRandomWinTitle :: MonadIO m => m T.Text
mkRandomWinTitle = (T.append "TaskbarInvisible:" . TS.showt) <$> liftIO getCPUTime
{-# INLINE mkRandomWinTitle #-}

-- | Показать простейшее диалоговое окно с текстом и одной кнопкой с помощью SDL. Для не ленивого Text.
showErrMsgBoxT :: T.Text -> IO ()
showErrMsgBoxT t = do
   appName <- getAppName
   SDL.showSimpleMessageBox Nothing SDL.Error (T.pack appName) t
{-# INLINE showErrMsgBoxT #-}

-- | Показать простейшее диалоговое окно с текстом и одной кнопкой с помощью SDL. Для ленивого Text.
showErrMsgBoxTL :: TL.Text -> IO ()
showErrMsgBoxTL = showErrMsgBoxT . TL.toStrict
{-# INLINE showErrMsgBoxTL #-}

-- | Показать простейшее диалоговое окно с текстом и одной кнопкой с помощью SDL.
-- Для билдера из TextShow.
showErrMsgBoxB :: TS.Builder -> IO ()
showErrMsgBoxB = showErrMsgBoxT . TS.toText
{-# INLINE showErrMsgBoxB #-}

-- | Показать простейшее диалоговое окно с текстом и одной кнопкой с помощью SDL. Для String.
showErrMsgBox :: String -> IO ()
showErrMsgBox = showErrMsgBoxT . T.pack
{-# INLINE showErrMsgBox #-}

-- | Обработчик не пойманных ранее исключений.
-- Обычное завершение программы по @exitSuccess@, @exitFailure@ пропускает, остальные исключения
-- передаёт в указанной функции.
guiCatch :: IO a -> (SomeException -> IO a) -> IO a
guiCatch f h = f `catches` [Handler (\ e -> throw (e :: ExitCode)),
                            Handler h]
