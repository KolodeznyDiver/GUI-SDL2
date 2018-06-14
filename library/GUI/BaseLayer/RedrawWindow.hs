{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      GUI.BaseLayer.RedrawWindow
-- Copyright:   (c) 2017-2018 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Функции перерисоки окна.

module GUI.BaseLayer.RedrawWindow(
     -- * Функции перерисоки окон.
     redrawWindowByIx,redrawWindow,redrawAll
                     ) where

import qualified SDL
import SDL.Vect
import Data.StateVar
import Data.Bits
import Control.Monad.IO.Class
import qualified Data.Vector as V
import Control.Monad
import Control.Monad.Extra (whenJust)
import GUI.BaseLayer.Depend0.Types
import GUI.BaseLayer.Depend0.Ref
import GUI.BaseLayer.Depend1.Geometry
import GUI.BaseLayer.Types
import GUI.BaseLayer.Widget
import qualified GUI.BaseLayer.Primitives as P
import GUI.BaseLayer.Canvas
import GUI.BaseLayer.Window
import GUI.BaseLayer.GUIRecord

-- | Перерисовать окно заданное по индексу 'GuiWindowIx'.
-- Флаги @WindowRedrawFlag@ и @WidgetRedrawFlag@ сьрасываются.
redrawWindowByIx:: MonadIO m =>
    -- | Ссылка на 'GUIRecord'
    Gui ->
    -- | Индекс перерисовываемого окна
    GuiWindowIx ->
    -- | Форсировать перерисовывание всего окна. Если False - перерисовываются только ветки с которых
    -- начинаются помеченные для перерисовки виджеты.
    Bool ->
    m ()
redrawWindowByIx gui ix force = doForWinByIx (`redrawWindow` force) gui ix
{-# INLINE redrawWindowByIx #-}

-- | Перерисовать окно.
-- Флаги @WindowRedrawFlag@ и @WidgetRedrawFlag@ сьрасываются.
redrawWindow:: MonadIO m =>
    -- | Перерисовываемое окно.
    Window ->
    -- | Форсировать перерисовывание всего окна. Если False - перерисовываются только ветки с которых
    -- начинаются помеченные для перерисовки виджеты.
    Bool ->
    m ()
redrawWindow rfWin force = do
    win <- readMonadIORef rfWin
    wSz <- P.fromSDLV2 <$> get (SDL.windowSize $ winSDL win)
    tSz <- P.getTextureSize $ winBuffer win
    let szChanged = wSz /= tSz
        force2 = force || szChanged
        gui = guiOfWindow win
    when (force2 || allWindowFlags' win WindowRedrawFlag) $ do
        windowFlagsRemove rfWin WindowRedrawFlag
        g <- readMonadIORef gui
--        liftIO $ putStrLn $ "redrawWindow : wSz=" ++ show wSz
        let renderer = winRenderer win
            rm = resourceManager g
            target = SDL.rendererRenderTarget renderer
            clip = SDL.rendererClipRect renderer
            go parentOff clipRect force3 widget = do
                w <- readMonadIORef widget
                let (SDL.Rectangle widgP widgSz) = widgetRect w
                    pInWinCoord = widgP .+^ parentOff
                    rect = rectIntersection clipRect $ SDL.Rectangle pInWinCoord widgSz
{-                liftIO $ putStrLn $ concat ["redrawWindow.go : parentOff=", show parentOff,
                    "  clipRect=", show clipRect, "  widgetRect=", show $ widgetRect w,
                    "  widgetCanvasRect=", show $ widgetCanvasRect w,
                    "  pInWinCoord=", show pInWinCoord, "   rect=", show rect] -}
                when ((widgetFlags w .&. WidgetVisible) /= WidgetNoFlags && not (isEmptyRect rect)) $ do
                        let markedForRedraw = isWidgetMarkedForRedrawing' w
                            force4 = force3 || markedForRedraw
                            off = pInWinCoord .-. getRectLT (widgetCanvasRect w)
                        when force4 $ do
                            when markedForRedraw $ clearWidgetRedrawFlag widget
                            clip $= Just (P.toSDLRect rect)
                            -- liftIO $ putStrLn $ concat ["redrawWindow clip rect=",show (P.toSDLRect rect)]
                            logOnErr gui "redrawWindow.onDraw" $
                               runCanvas renderer rm (winTextureCache win) off $ -- do
                                    (onDraw $ widgetFns w) widget
--                                visibleRect <- getVisibleRect widget
--                                setColor $ V4 255 0 0 0
--                                drawRect $ shrinkRect' 1 visibleRect

                        V.mapM_  (go off rect force4) (cildrenWidgets w)
--                liftIO $ putStrLn $ concat ["redrawWindow.go END "]
        let do' bufSzChanged oBuf newSz force' widget' off fieldModifyFun = do
                buf <-  if bufSzChanged then do
                            SDL.destroyTexture oBuf
                            newBuf <- P.createTargetTexture renderer newSz
                            modifyMonadIORef' rfWin (fieldModifyFun newBuf)
                            return newBuf
                        else return oBuf
                target $= Just buf
                go off (SDL.Rectangle zero newSz) force' widget'
                return buf
        mbFgChildren <- getWidgetChild (winFgWidget win) 0
        mbFg <- case mbFgChildren of
                    Just fgChildren -> do
                        rect@(SDL.Rectangle (P off) sz) <- getWidgetRect fgChildren
                        oSz <- P.getTextureSize $ winFgBuffer win
                        let fgForce = sz /= oSz
                        buf <- do' fgForce (winFgBuffer win) sz (force || fgForce) fgChildren
                                    (fmap negate off) $ \buf w -> w{winFgBuffer=buf}
                        return $ Just (buf,rect)
                    _ -> return Nothing
        buf <- do' szChanged (winBuffer win) wSz force2 (mainWidget win) zero
                        $ \buf w -> w{winBuffer=buf}
        clip   $= Nothing
        target $= Nothing
        SDL.clear renderer
        SDL.copy renderer buf Nothing  Nothing -- $ Just $ winRect
        whenJust mbFg $ \ (buf',rect) ->
            SDL.copy renderer buf' Nothing $ Just (fmap fromIntegral rect)
        SDL.present renderer

-- | Перерисовать все окна, нефорсировано.
redrawAll :: MonadIO m => Gui -> m ()
redrawAll = allWindowsMap_ (`redrawWindow` False)
{-# INLINE redrawAll #-}


