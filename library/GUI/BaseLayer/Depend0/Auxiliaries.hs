{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:      GUI.BaseLayer.Depend0.Auxiliaries
-- Copyright:   (c) 2017-2018 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Функции @showErrMsgBox*@ из этого модуля непосредственно обращаются к SDL минуя окна и виджеты GUI.
-- Они практически не имеют настроек и не имеют поддержки 'Skin'. Их рекомендуется использовать только в случае, если
-- мы не уверены что GUI в рабочем состоянии. Иначе следует использовать функции из модуля "GUI.Window.MessageBox"

module GUI.BaseLayer.Depend0.Auxiliaries(
    -- * Некоторые вспомогательные функции.
    getAppName,withUTF8,withUtf8Locale,mkRandomWinTitle
    ,showErrMsgBoxT,showErrMsgBoxTL,showErrMsgBoxB,showErrMsgBox,guiCatch
    ,showbV2,showbPoint
    ) where

import System.Environment (getProgName)
import Control.Exception.Safe
import GHC.IO.Encoding
import System.Exit
import System.CPUTime
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified TextShow as TS
import           TextShow (showb)
import System.FilePath
import qualified SDL
import SDL.Vect

-- | Возвращает имя текущего исполняемого файла без расширения и пути.
getAppName :: IO String
getAppName = takeBaseName <$> getProgName
{-# INLINE getAppName #-}

-- | Для переданной функции временно устанавливает режим кодировки @setForeignEncoding utf8@.
withUTF8 ::  MonadIO m => m a -> m a
withUTF8 f = do
    save <- liftIO (getForeignEncoding <* setForeignEncoding utf8)
    r <- f
    liftIO $ setForeignEncoding save
    return r
{-# INLINE withUTF8 #-}

-- | Для переданной функции временно устанавливает режим кодировки @setLocaleEncoding utf8@.
withUtf8Locale ::  MonadIO m => m a -> m a
withUtf8Locale f = do
    save <- liftIO (getLocaleEncoding <* setLocaleEncoding utf8)
    r <- f
    liftIO $ setLocaleEncoding save
    return r
{-# INLINEABLE withUtf8Locale #-}


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

-- guiCatch :: MonadCatch m => m a -> (SomeException -> m a) -> m a
-- guiCatch = catchAny

showbV2 :: TS.TextShow a => V2 a -> TS.Builder
showbV2 (V2 x y) = "(" <> showb x <> "," <> showb y <> ")"

showbPoint  :: TS.TextShow a => SDL.Point V2 a -> TS.Builder
showbPoint (P p) = "P" <> showbV2 p