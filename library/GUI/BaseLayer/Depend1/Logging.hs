{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:      GUI.BaseLayer.Depend1.Logging
-- Copyright:   (c) 2017-2018 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Журнал GUI с возможностью вывода сообщений и/или в файл и на консоль (терминал).
-- Логирование необработанных в пользовательском коде Exception.

module GUI.BaseLayer.Depend1.Logging(
    -- * Журнал GUI приложения.
    LogDateTime(..),GUILogDef(..),GUILog,guiLogStart,guiLogStop,logPutLn
    ,logOnSomeException,logOnErr
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Exception.Safe
import Data.IORef
import Data.Time
import           System.Directory
import           System.FilePath
import           System.IO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TIO
import qualified TextShow as TS
import           TextShow (showb)
import Control.Monad.Extra (whenJust,unlessM)
import Data.Default
import GUI.BaseLayer.Depend0.Ref
import GUI.BaseLayer.Depend0.Auxiliaries

-- | Опция вывода даты/времени в файл журнала.
data LogDateTime = LogNoDateTime -- ^ Ни дата, ни время не выводятся
                 | LogTimeOnly -- ^ Только время
                 | LogDateAndTime -- ^ Выводятся дата и время.
                 deriving (Eq)

-- | Параметры логирования. Тип передаётся в качестве аргумента функции @GUI.BaseLayer.RunGUI.runGUI@.
data GUILogDef = GUILogDef {
    logFileName :: String -- ^ Файл для вывода журнала.
                          -- Пустая строка - журнал не выводится в файл.
                          -- Если задан относительный путь к файлу, то файл создаётся в директории
                          -- заданной как аргумент функции @guiLogStart@.
    , logDateTimeMode :: LogDateTime -- ^ Опция вывода даты/времени в файл журнала.
    , logFileAppend :: Bool -- Если True, файл журнала не очищается при очередном старте приложения.
    }

instance Default GUILogDef where
    def = GUILogDef { logFileName = ""
                    , logDateTimeMode = LogDateAndTime
                    , logFileAppend = False
                    }

-- no export
data GUILog' = GUILog'  { logFName :: TS.Builder
                        , logDateTime :: LogDateTime
                        , logTZ :: TimeZone
                        , logWriteFail :: IORef Bool
                        , logHandle :: Handle
                        }

-- | Данные текущего логирования.
data GUILog = GUILog {
      logGUILog' :: Maybe GUILog' -- ^ Если файл создан, то параметры вывода.
    , logOutToConsole :: Bool -- ^ Разрешён ли вывод в консоль (терминал).
                     }

-- | Функция должна быть вызвана для начала логирования.
guiLogStart :: GUILogDef -> -- ^ Параметры логирования.
               String -> -- ^ Путь к директории журнала если в @logFileName@ указан относительный путь.
               Bool -> -- ^ Выводить ли сообщения в консоль (терминал).
               IO (Maybe GUILog)
guiLogStart GUILogDef{..} dataDirectory outToConsole =
    if null logFileName then return $ Just (GUILog Nothing outToConsole)
    else do
        let path = if isRelative logFileName then dataDirectory </> logFileName
                   else logFileName
        r <- try $ do
                createDirectoryIfMissing True $ takeDirectory path
                h <- openFile path (if logFileAppend then AppendMode else WriteMode)
                hSetBuffering h (BlockBuffering (Just 4096))
                return h
        case r of
              Left  e -> do
                showErrMsgBox $ concat ["Can't create logfile ", path," : ",
                                     show (e :: IOException)]
                return Nothing
              Right h -> do
                tz <- getCurrentTimeZone -- НЕ вызывать при перенастроенном setForeignEncoding
                bWriteFail <- newIORef False
                return $ Just (GUILog (Just (GUILog' (TS.fromString path) logDateTimeMode tz bWriteFail h))
                                    outToConsole)

-- | Функция должна быть вызвана для завершения логирования при завершении приложения.
guiLogStop :: GUILog -> IO ()
guiLogStop GUILog{..} =
    whenJust logGUILog' $ \GUILog'{..} -> do
      unlessM (readMonadIORef logWriteFail) $
        hFlush logHandle
      hClose logHandle

-- | Вывод сообщения в журнал.
logPutLn :: MonadIO m =>
    GUILog -> -- ^ Журнал.
    TS.Builder -> -- ^ TextShow Билдер из пакета __text-show__.
    m ()
logPutLn GUILog{..} msg' = do
    let msg = let t = TS.toLazyText msg' in
              TS.fromLazyText (if TL.null t || (TL.last t /= '\n') then t else TL.tail t)
    when logOutToConsole $
        liftIO $ TIO.putStrLn $ TS.toLazyText msg
    whenJust logGUILog' $ \GUILog'{..} ->
      unlessM (readMonadIORef logWriteFail) $ do
        let addT frmt = ((<> msg) . TS.fromString .
                            formatTime defaultTimeLocale frmt . utcToLocalTime logTZ)
                             <$> liftIO getCurrentTime
        t <- case logDateTime of
                LogNoDateTime  -> return msg
                LogTimeOnly    -> addT     "%X   "
                LogDateAndTime -> addT "%x  %X   "
        liftIO $ handle (\e -> do
                            writeIORef logWriteFail True
                            let errMsg = "Can't write to logfile " <> logFName <> " : " <>
                                                showb (e :: IOException)
                            when logOutToConsole $
                                TIO.putStrLn $ TS.toLazyText errMsg
                            showErrMsgBoxB errMsg
                        ) $ do
            TIO.hPutStrLn logHandle $ TS.toLazyText t
            hFlush logHandle

-- | Вывод информации об исключении. Функция должна вызываться из обработчика исключения.
logOnSomeException :: GUILog -> -- ^ Журнал.
                      TS.Builder -> -- ^ Строка выводимая перед текстом исключения.
                      SomeException -> -- ^ Исключение.
                      IO ()
logOnSomeException gLog t e = logPutLn gLog $ t <> " : " <> showb e
{-# INLINE logOnSomeException #-}

-- | Выполнение некоторого действия, и, если, при этом возникнет исключение, вывести его в журнал.
logOnErr :: MonadIO m => GUILog -> TS.Builder -> IO () -> m ()
logOnErr gLog t f = liftIO $ guiCatch f (logOnSomeException gLog t)
{-# INLINE logOnErr #-}

