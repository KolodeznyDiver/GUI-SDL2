{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module GUI.BaseLayer.Logging(
    LogDateTime(..),GUILogDef(..),GUILog,guiLogStart,guiLogStop,logPutLn
    ,logOnSomeException,logOnErr
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Exception
import Data.IORef
import Data.Time
import           System.Directory
import           System.FilePath
import           System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
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
data GUILog' = GUILog'  { logFName :: String
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
                return $ Just (GUILog (Just (GUILog' path logDateTimeMode tz bWriteFail h))
                                    outToConsole)

guiLogStop :: GUILog -> IO ()
guiLogStop GUILog{..} =
    whenIsJust logGUILog' $ \GUILog'{..} -> do
      unlessM (readMonadIORef logWriteFail) $
        hFlush logHandle
      hClose logHandle

logPutLn :: MonadIO m => GUILog -> T.Text -> m ()
logPutLn GUILog{..} msg' = do
    let msg = if T.null msg' || (T.last msg' /= '\n') then msg' else T.tail msg'
    when logOutToConsole $
        liftIO $ TIO.putStrLn msg
    whenIsJust logGUILog' $ \GUILog'{..} ->
      unlessM (readMonadIORef logWriteFail) $ do
        let addT frmt = ((`T.append` msg) . T.pack . formatTime defaultTimeLocale frmt . utcToLocalTime logTZ)
                             <$> liftIO getCurrentTime
        t <- case logDateTime of
                LogNoDateTime  -> return msg
                LogTimeOnly    -> addT     "%X   "
                LogDateAndTime -> addT "%x  %X   "
        liftIO $ handle (\e -> do
                            writeIORef logWriteFail True
                            let errMsg = concat ["Can't write to logfile ", logFName, " : ",
                                                show (e :: IOException)]
                            when logOutToConsole $
                                putStrLn errMsg
                            showErrMsgBox errMsg
                        ) $ do
            TIO.hPutStrLn logHandle t
            hFlush logHandle

logOnSomeException :: GUILog -> T.Text -> SomeException -> IO ()
logOnSomeException gLog t = logPutLn gLog . mkT . show
    where mkT s = T.concat [t," : ",T.pack s]
{-# INLINE logOnSomeException #-}

logOnErr :: MonadIO m => GUILog -> T.Text -> IO () -> m ()
logOnErr gLog t f = liftIO $ guiCatch f (logOnSomeException gLog t)
{-# INLINE logOnErr #-}

