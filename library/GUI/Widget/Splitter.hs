{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module:      GUI.Widget.Splitter
-- Copyright:   (c) 2017-2018 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Сплиттер - невидимый в обыяных условиях виджет вставляемый между другими виджетами в
-- горизонтальном @hLayout@ или вертикальном @vLayout@ лайауте.
-- При наведении указателя в область сплиттера вид курсора меняется. При удержании ЛКМ появляется возможность
-- перемещать позицию сплиттера. Для горизонтального лайаута сплиттер измешняет ширину виджета левее себя,
-- для вертикального лайаута изменяет высоту виджета выше себя.
-- Сплиттер сам определяет тип лайаута.

module GUI.Widget.Splitter(
        SplitterData,splitter
        ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Ix
import Data.Bits
import qualified SDL
import SDL.Vect
import GUI
import GUI.Widget.Handlers

pattern SplitterWidth :: Coord
pattern SplitterWidth = 6

-- | Не экспортируется. Тип перемещения (тип лайаута). Используется в @splitter@.
data SplitterType = SplitterUnknown | HSplitter | VSplitter

-- | Тип созданного виджета. Обычно используется как  @GuiWidget SplitterData@.
data SplitterData = SplitterData

-- | Функция создания сплиттера. Сплиттер не имеет своих специальных параметров.
splitter :: MonadIO m => Widget -> -- ^ Будущий предок в дереве виджетов.
                         Skin -> -- ^ Skin.
                         m (GuiWidget SplitterData)
splitter parent skin = do
    let noInitedSz = V2 1 1
        detectSplitterType (SDL.Rectangle (P (V2 xd yd)) (V2 wd hd)) -- drivenR
                           (SDL.Rectangle (P (V2 x y)) _)     -- selfR
                            | wd <0 || hd <0 {- || (x == 0 && y == 0) -} = SplitterUnknown
                            | otherwise = let left' = (xd+wd)<=x
                                              up' = (yd+hd)<=y in
                                          if left' && not up' then HSplitter
                                          else if not left' && up' then VSplitter
                                               else SplitterUnknown
--        prepare :: MonadIO m => Widget -> GuiRect -> m Bool
        prepare widget selfR@(SDL.Rectangle _ (V2 w h)) = do
            mbDriven <- getPrevNextWidgets widget
--            liftIO $ putStrLn $ concat ["splitter.prepare selfR=", rectToBriefStr selfR,
--               "  isJust widget0= ", show $ isJust $ fst mbDriven,"  isJust widget1= ", show $ isJust $ snd mbDriven]
            case mbDriven of
                (Just widget0,_) -> do
                    drivenR@(SDL.Rectangle _ (V2 wd hd)) <- getWidgetRectWithMargin widget0
                    curIx <- getWidgetCursorIx widget
--                    liftIO $ putStrLn $ concat ["splitter.prepare drivenR=",rectToBriefStr  drivenR]
                    let t = case curIx of
                                SystemCursorSizeWE -> HSplitter
                                SystemCursorSizeNS -> VSplitter
                                _ -> detectSplitterType drivenR selfR
                    case t of
                        HSplitter -> do
                            when (curIx /=SystemCursorSizeWE) $
                                setWidgetCursorIx widget SystemCursorSizeWE
                            let n = h /= hd || w /= SplitterWidth
                            when n $ notifyParentAboutSize widget (V2 SplitterWidth hd)
                            return n
                        VSplitter -> do
                            when (curIx /=SystemCursorSizeNS) $
                                setWidgetCursorIx widget SystemCursorSizeNS
                            let n = w /= wd || h /= SplitterWidth
                            when n $ notifyParentAboutSize widget (V2 wd SplitterWidth)
                            return n
                        _ -> return False
                _ -> return False

        fns = noChildrenFns noInitedSz
    mkWidget (WidgetVisible .|. WidgetEnable) WidgetMarginNone SplitterData parent fns{
        onMouseButton = \widget motion mouseButton _ {-clicks -} _point -> do
            ena <- allWidgetFlags widget WidgetEnable
            when (ena && (mouseButton == SDL.ButtonLeft)) $ do
                if motion==SDL.Pressed then
                    setMouseCapturedWidget widget
                else resetMouseCapturedWidget widget
                markWidgetForRedraw widget
        ,onMouseMotion = \widget btnsLst (P (V2 x y)) (V2 dx dy) ->
            when ([SDL.ButtonLeft] == btnsLst ) $ do
                mbDriven <- getPrevNextWidgets widget
                case mbDriven of
                  (Just widget0, Just widget1) -> do
                    (SDL.Rectangle _ (V2 w0 h0)) <- getWidgetRect widget0
                    (V2 mw0 mh0) <- getWidgetMarginSize widget0
                    (SDL.Rectangle _ (V2 w1 h1)) <- getWidgetRect widget1
                    (V2 mw1 mh1) <- getWidgetMarginSize widget1
                    -- marg1 <- getWidgetMargin widget1
                    (SDL.Rectangle _ (V2 wp hp)) <- getWidgetRect =<< getWidgetParent widget
                    (SDL.Rectangle _ (V2 selfW selfH)) <- getWidgetRect widget
--                    liftIO $ putStrLn $ concat ["splitter.onMouseMotion rp=", rectToBriefStr rp,
--                      "  r0=", rectToBriefStr r0,"  r1=", rectToBriefStr r1]
                    curIx <- getWidgetCursorIx widget
                    case curIx of
                        SystemCursorSizeWE -> -- HSplitter
                            if inRange (0,selfH) y then
                                let newW = toBound 0 (wp - SplitterWidth - mw0 - mw1) $ w0+dx in
                                when (w0 /= newW) $ do
                                    notifyParentAboutSize widget0 (V2 newW h0)
                                    notifyParentAboutSize widget1 (V2 (wp-newW-SplitterWidth - mw0 - mw1) h1)
                                    markWidgetForRedraw widget
                            else resetMouseCapturedWidget widget >> markWidgetForRedraw widget
                        SystemCursorSizeNS -> -- VSplitter
                            if inRange (0,selfW) x then
                                let newH = toBound 0 (hp - SplitterWidth - mh0 - mh1) $ h0+dy in
                                when (h0 /=newH) $ do
                                    notifyParentAboutSize widget0 (V2 w0 newH)
                                    notifyParentAboutSize widget1 (V2 w1 (hp-newH-SplitterWidth - mh0 - mh1))
                                    markWidgetForRedraw widget
                            else resetMouseCapturedWidget widget >> markWidgetForRedraw widget
                        _ -> return ()
                  _ -> return ()
        ,onResizing= \widget newRect -> do
            chgd <- prepare widget newRect
            unless chgd $ onResizing fns widget newRect
{-        ,onLostMouseFocus = \widget -> do
            mbC <- getMouseCapturedWidget =<< getWidgetWindow widget
            when (mbC == Just widget) $ -}
        ,onDraw= \widget -> do
            r <- getVisibleRect widget
            mbC <- getMouseCapturedWidget =<< getWidgetWindow widget
            if mbC == Just widget then do
                let DecoreState{..} = splitterActive skin
                setColor decoreBkColor
                fillRect r
                setColor decoreFgColor
                fillRect $ shrinkRect' 2 r
            else do
                setColor $ decoreBkColor (formDecore skin)
                fillRect r
--            liftIO $ putStrLn $ concat ["label.onDraw getVisibleRect=", rectToBriefStr r]
                            }