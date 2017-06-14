{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module GUI.BaseLayer.Logging(
    LogDateTime(..),GUILogDef(..),GUILog,guiLogStart,guiLogStop,logPutLn
    ,logOnSomeException,logOnErr
    ) where

import Data.Monoid
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception
import Data.IORef
import Data.Time
import           System.Directory
import           System.FilePath
import           System.IO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TIO
import qualified TextShow as TS
import           TextShow (showb)
import Maybes (whenIsJust)
import MonadUtils (unlessM)
import Data.Default
import GUI.BaseLayer.Ref
import GUI.BaseLayer.Auxiliaries

data LogDateTime = LogNoDateTime
                 | LogTimeOnly
                 | LogDateAndTime
                 deriving (Eq)

data GUILogDef = GUILogDef { logFileName :: String
                           , logDateTimeMode :: LogDateTime
                           , logFileAppend :: Bool
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

data GUILog = GUILog { logGUILog' :: Maybe GUILog'
                     , logOutToConsole :: Bool
                     }

guiLogStart :: GUILogDef -> String -> Bool -> IO (Maybe GUILog)
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

guiLogStop :: GUILog -> IO ()
guiLogStop GUILog{..} =
    whenIsJust logGUILog' $ \GUILog'{..} -> do
      unlessM (readMonadIORef logWriteFail) $
        hFlush logHandle
      hClose logHandle

logPutLn :: MonadIO m => GUILog -> TS.Builder -> m ()
logPutLn GUILog{..} msg' = do
    let msg = let t = TS.toLazyText msg' in
              TS.fromLazyText (if TL.null t || (TL.last t /= '\n') then t else TL.tail t)
    when logOutToConsole $
        liftIO $ TIO.putStrLn $ TS.toLazyText msg
    whenIsJust logGUILog' $ \GUILog'{..} ->
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

logOnSomeException :: GUILog -> TS.Builder -> SomeException -> IO ()
logOnSomeException gLog t e = logPutLn gLog $ t <> " : " <> showb e
{-# INLINE logOnSomeException #-}

logOnErr :: MonadIO m => GUILog -> TS.Builder -> IO () -> m ()
logOnErr gLog t f = liftIO $ guiCatch f (logOnSomeException gLog t)
{-# INLINE logOnErr #-}

