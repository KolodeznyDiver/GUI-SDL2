module GUI.BaseLayer.Auxiliaries(
    getAppName,showErrMsgBoxT,showErrMsgBoxTL,showErrMsgBoxB,showErrMsgBox,guiCatch
    ) where

import System.Environment (getProgName)
import Control.Exception
import System.Exit
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified TextShow as TS
import System.FilePath
import qualified SDL

getAppName :: IO String
getAppName = takeBaseName <$> getProgName
{-# INLINE getAppName #-}

showErrMsgBoxT :: T.Text -> IO ()
showErrMsgBoxT t = do
   appName <- getAppName
   SDL.showSimpleMessageBox Nothing SDL.Error (T.pack appName) t
{-# INLINE showErrMsgBoxT #-}

showErrMsgBoxTL :: TL.Text -> IO ()
showErrMsgBoxTL = showErrMsgBoxT . TL.toStrict
{-# INLINE showErrMsgBoxTL #-}

showErrMsgBoxB :: TS.Builder -> IO ()
showErrMsgBoxB = showErrMsgBoxT . TS.toText
{-# INLINE showErrMsgBoxB #-}

showErrMsgBox :: String -> IO ()
showErrMsgBox = showErrMsgBoxT . T.pack
{-# INLINE showErrMsgBox #-}

guiCatch :: IO a -> (SomeException -> IO a) -> IO a
guiCatch f h = f `catches` [Handler (\ e -> throw (e :: ExitCode)),
                            Handler h]
