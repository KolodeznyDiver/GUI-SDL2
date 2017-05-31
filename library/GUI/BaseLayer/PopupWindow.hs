{-# LANGUAGE RecordWildCards #-}
module GUI.BaseLayer.PopupWindow(
    mkPopupWindow
    ) where

import Control.Monad.IO.Class -- (MonadIO)
import Data.Bits
import qualified Data.Text as T
import qualified SDL
import SDL.Vect
import GUI.BaseLayer.Types
import GUI.BaseLayer.Internal.Types
import GUI.BaseLayer.Widget
import GUI.BaseLayer.Window
import qualified GUI.BaseLayer.Primitives as P
import GUI.BaseLayer.Utils

mkPopupWindow :: MonadIO m => Widget ->  GuiRect  -> m GuiWindow
mkPopupWindow widget (SDL.Rectangle (P p) sz) = do
    win <- getWidgetWindow widget
    gui <- getGuiFromWindow win
    offset <- getWidgetCoordOffset widget
    winSDL <- getSDLWindow win
    absPos <- SDL.getWindowAbsolutePosition winSDL
    newWindow' gui T.empty (WindowRedrawFlag .|. WindowCloseOnLostFocuse .|. WindowPopupFlag)
            $ SDL.defaultWindow { SDL.windowInitialSize = P.toSDLV2 sz
                                , SDL.windowBorder = False
                                , SDL.windowPosition = SDL.Absolute $ P $ P.toSDLV2 (p ^+^ offset) ^+^ absPos
                                }
