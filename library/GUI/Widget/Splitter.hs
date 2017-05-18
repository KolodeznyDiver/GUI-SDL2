-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
module GUI.Widget.Splitter(
        SplitterData,splitter
        ) where

import Control.Monad
import Control.Monad.IO.Class
--import Data.Maybe
import Data.Ix
import Data.Bits
--import Data.IORef
import Maybes (whenIsJust)
import qualified SDL
import SDL.Vect
import GUI
--import GUI.Widget.Handlers
--import GUI.BaseLayer.Geometry
--import GUI.BaseLayer.Skin
--import GUI.Widget.Types
--import qualified GUI.BaseLayer.Primitives as P
--import Data.Default

pattern SplitterWidth :: Coord
pattern SplitterWidth = 6

data SplitterType = SplitterUnknown | HSplitter | VSplitter

data SplitterData = SplitterData


splitter :: MonadIO m => Widget -> Skin -> m (GuiWidget SplitterData)
splitter parent skin = do
--    colorRf <- newMonadIORef $ fromMaybe (foregroundColor skin) labelColor
    let noInitedSz = V2 1 1
        detectSplitterType (SDL.Rectangle (P (V2 xd yd)) (V2 wd hd)) -- drivenR
                           (SDL.Rectangle (P (V2 x y)) _)     -- selfR
                            | wd <=0 || hd <=0 || (x == 0 && y == 0) = SplitterUnknown
                            | otherwise = let left' = (xd+wd)<=x
                                              up' = (yd+hd)<=y in
                                          if left' && not up' then HSplitter
                                          else if not left' && up' then VSplitter
                                               else SplitterUnknown
--        prepare :: MonadIO m => Widget -> GuiRect -> m Bool
        prepare widget selfR@(SDL.Rectangle _ (V2 w h)) = do
            mbDriven <- getPrevWidget widget
            case mbDriven of
                Just driven -> do
                    drivenR@(SDL.Rectangle _ (V2 wd hd)) <- getWidgetRectWithMargin driven
                    curIx <- getWidgetCursorIx widget
                    let t = case curIx of
                                SystemCursorSizeWE -> HSplitter
                                SystemCursorSizeNS -> VSplitter
                                _ -> detectSplitterType drivenR selfR
                    case t of
                        HSplitter -> do
                            when (curIx /=SystemCursorSizeWE) $
                                setWidgetCursorIx widget SystemCursorSizeWE
                            let n = h /= hd || w /= SplitterWidth
                            when n $ notifyParentSizeWithMargin widget (V2 SplitterWidth hd)
                            return n
                        VSplitter -> do
                            when (curIx /=SystemCursorSizeNS) $
                                setWidgetCursorIx widget SystemCursorSizeNS
                            let n = w /= wd || h /= SplitterWidth
                            when n $ notifyParentSizeWithMargin widget (V2 wd SplitterWidth)
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
                (V2 selfW selfH) <- sizeOfRect <$> getWidgetRect widget
                mbDriven <- getPrevWidget widget
                whenIsJust mbDriven $ \driven -> do
                    (V2 xd yd) <- sizeOfRect <$> getWidgetRect driven
                    curIx <- getWidgetCursorIx widget
                    case curIx of
                        SystemCursorSizeWE -> do -- HSplitter
                            if inRange (0,selfH) y then
                                notifyParentSizeWithMargin driven (V2 (max 1 (xd+dx)) yd)
                            else resetMouseCapturedWidget widget
                            markWidgetForRedraw widget
                        SystemCursorSizeNS -> do -- VSplitter
                            if inRange (0,selfW) x then
                                notifyParentSizeWithMargin driven (V2 xd (max 1 (yd+dy)))
                            else resetMouseCapturedWidget widget
                            markWidgetForRedraw widget
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
                setColor $ bkColor skin
                fillRect r
--            liftIO $ putStrLn $ concat ["label.onDraw getVisibleRect=", rectToBriefStr r]
                            }