-- |
-- Module:      GUI.BaseLayer.PopupWindow
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Поддержка всплывающих окон (popup window).
--
-- В контексте данного GUI под popup окнами подразумеваются неглавные окна приложения, которые
-- автоматически закрываются при выборе другого, не popup окна. То есть это окна всплывающих меню,
-- хинтов (всплывающих подсказок) и подобное. Они не имеют рамок и заголовков.

module GUI.BaseLayer.PopupWindow(
    mkPopupWindow
    ) where

import Control.Monad.IO.Class
import Data.Bits
import qualified Data.Text as T
import qualified SDL
import SDL.Vect
import GUI.BaseLayer.Depend0.Types
import GUI.BaseLayer.Types
import GUI.BaseLayer.Widget
import GUI.BaseLayer.Window
import qualified GUI.BaseLayer.Primitives as P
import GUI.BaseLayer.Core
import System.Utils

-- | Создать всплывающее окно.
mkPopupWindow :: MonadIO m =>
    -- | Виджет активного сейчас окна. Popup окно станет дочерним по отношению к этому окну.
    Widget ->
    -- | Начальные координаты окна в координатах указанного виджета.
    GuiRect  ->
    -- | Созданное Popup окно в котором можно строить дерево виджетов как в обычном.
    m Window
mkPopupWindow widget (SDL.Rectangle (P p) sz) = do
    win <- getWidgetWindow widget
    windowFlagsAdd win WindowWaitPopup
    gui <- getWindowGui win
    offset <- getWidgetCoordOffset widget
    absPos <- SDL.getWindowAbsolutePosition =<< getSDLWindow win
    borderOff <- if IsWindows then return $ V2 0 0
                 else do
                    (V4 l _r t _b) <- getWinBorders
--                    liftIO $ putStrLn $ concat ["mkPopupWindow  getWinBorders=", show v4]
                    return $ V2 l t             
--    borderOff <- getWindowBordersSize =<< getSDLWindow win 
--    liftIO $ putStrLn $ concat ["mkPopupWindow p=",show p," offset=", show offset, " absPos=", show absPos
--          , "  borderOff=", show borderOff
--          ]  
    newWindow' gui T.empty
        (WindowRedrawFlag .|. WindowCloseOnLostFocuse .|. WindowPopupFlag .|.
         WindowCloseOnEsc .|. WindowWaitPopupReset) $
        SDL.defaultWindow { SDL.windowInitialSize = P.toSDLV2 sz
                          , SDL.windowBorder = False
                          , SDL.windowPosition = SDL.Absolute $ P $ P.toSDLV2 (p ^+^ offset) ^+^ absPos ^+^ borderOff
                          }
