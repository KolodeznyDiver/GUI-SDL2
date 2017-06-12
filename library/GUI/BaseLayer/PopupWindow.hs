{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module GUI.BaseLayer.PopupWindow(
    mkPopupWindow
    ) where

import Control.Monad.IO.Class
import Data.Bits
import System.CPUTime
import qualified Data.Text as T
import qualified SDL
import SDL.Vect
import GUI.BaseLayer.Types
import GUI.BaseLayer.Internal.Types
import GUI.BaseLayer.Widget
import GUI.BaseLayer.Window
import qualified GUI.BaseLayer.Primitives as P
import GUI.BaseLayer.Utils
import System.Utils (withRemoveFromTaskbar)

mkPopupWindow :: MonadIO m => Widget ->  GuiRect  -> m GuiWindow
mkPopupWindow widget (SDL.Rectangle (P p) sz) = do
    title <- (T.append "TaskbarInvisible:" . T.pack . show) <$> liftIO getCPUTime
    win <- getWidgetWindow widget
    gui <- getWindowGui win
    offset <- getWidgetCoordOffset widget
    winSDL <- getSDLWindow win
    absPos <- SDL.getWindowAbsolutePosition winSDL
    withRemoveFromTaskbar (T.unpack title) $ newWindow' gui title
        (WindowRedrawFlag .|. WindowCloseOnLostFocuse .|. WindowPopupFlag) $
        SDL.defaultWindow { SDL.windowInitialSize = P.toSDLV2 sz
                          , SDL.windowBorder = False
                          , SDL.windowPosition = SDL.Absolute $ P $ P.toSDLV2 (p ^+^ offset) ^+^ absPos
                          }
