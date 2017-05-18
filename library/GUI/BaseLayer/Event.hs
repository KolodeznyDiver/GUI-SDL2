module GUI.BaseLayer.Event(RedrawRequestMode(..),redrawRequestByWinId,redrawRequestByWidget) where

import qualified SDL
import qualified SDL.Raw as Raw
--import Control.Monad
--import SDL.Exception
import Foreign
-- import Foreign.C.Types
import GUI.BaseLayer.Types
import GUI.BaseLayer.Ref
import GUI.BaseLayer.Widget
import GUI.BaseLayer.Window
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import GUI.BaseLayer.Internal.Types

data RedrawRequestMode = RedrawRequestWidget | RedrawRequestWindow | RedrawRequestAll
        deriving (Eq, Enum, Show)

redrawRequestByWinId:: MonadIO m => Gui -> GuiWindowId -> RedrawRequestMode -> m ()
redrawRequestByWinId gui winId rrmode = do
    eType <- userEventCodeBase <$> readMonadIORef gui
    timestamp <- SDL.ticks
    Control.Monad.void $ liftIO $ with (Raw.UserEvent eType timestamp winId
        (fromIntegral $ fromEnum rrmode) nullPtr nullPtr) Raw.pushEvent

redrawRequestByWidget:: MonadIO m => Widget -> RedrawRequestMode -> m ()
redrawRequestByWidget widget rrmode = do
    widgStruct <- readMonadIORef widget
    win <- readMonadIORef $ windowOfWidget widgStruct
    winId <- getWinId' win
    when (rrmode == RedrawRequestWidget) $ markWidgetForRedraw widget
    redrawRequestByWinId (guiOfWindow win) winId rrmode

