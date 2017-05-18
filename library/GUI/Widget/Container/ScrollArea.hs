{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}         --  DisambiguateRecordFields
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
module GUI.Widget.Container.ScrollArea(
    ScrollAreaDef(..),ScrollAreaData,scrollArea
    ) where

import Control.Monad
import Control.Monad.IO.Class
--import qualified Data.Vector as V
import Data.Maybe
import Data.Bits
--import Data.IORef
import Maybes (whenIsJust)
import qualified SDL
import SDL.Vect
import GUI
--import GUI.BaseLayer.Skin
--import GUI.BaseLayer.Geometry
import GUI.Widget.Button
--import qualified GUI.BaseLayer.Primitives as P
import Data.Default
--import GUI.BaseLayer.BitFlags

import GUI.Widget.LinearTrackBar

{-
data ScrllAreaTag
type ScrollAreaFlags = Flags ScrllAreaTag

pattern ScrollAreaNoFlags :: ScrollAreaFlags
pattern ScrollAreaLeftArrVisible :: ScrollAreaFlags
pattern ScrollAreaUpArrVisible :: ScrollAreaFlags
pattern ScrollAreaRBArrAsR :: ScrollAreaFlags
pattern ScrollAreaRBArrAsD :: ScrollAreaFlags
--pattern WidgetTabbed :: ScrollAreaFlags
-- pattern WidgetMarkForPaint :: WidgetFlags
                                    --  5432109876543210
pattern ScrollAreaNoFlags         = (Flags 0x0000000000000000) :: ScrollAreaFlags
pattern ScrollAreaLeftArrVisible  = (Flags 0x0000000000000001) :: ScrollAreaFlags
pattern ScrollAreaUpArrVisible    = (Flags 0x0000000000000002) :: ScrollAreaFlags
pattern ScrollAreaRBArrAsR        = (Flags 0x0000000000000004) :: ScrollAreaFlags
pattern ScrollAreaRBArrAsD        = (Flags 0x0000000000000008) :: ScrollAreaFlags
-}

data ScrollAreaDef = ScrollAreaDef  { scrollAreaItemDef  :: FormItemWidgetDef
                                    , scrollAreaSize      :: GuiSize
                                    , scrollAreaFlags     :: WidgetFlags
                                    , scrollAreaSlidersColor :: Maybe GuiColor
                                    , scrollAreaArrowsColor :: Maybe GuiColor
                                    , scrollAreaSteps :: GuiSize
                                    }
                                    deriving (Show)

instance Default ScrollAreaDef where
    def = ScrollAreaDef { scrollAreaItemDef = def
                        , scrollAreaSize = V2 (-1) (-1)
                        , scrollAreaFlags = WidgetVisible .|. WidgetEnable .|. WidgetMouseWheelControl
                        , scrollAreaSlidersColor = Nothing
                        , scrollAreaArrowsColor = Nothing
                        , scrollAreaSteps = V2 100 100
                        }
{-
newtype ScrollAreaStruct = ScrollAreaStruct    { scrllArFlags :: ScrollAreaFlags
                                               }

newtype ScrollAreaData = ScrollAreaData { scrllAr :: IORef ScrollAreaStruct }
-}

data ScrollAreaData = ScrollAreaData

data ScrollAreaChilds = Scrolled | HBar | VBar | ArrL | ArrU | ArrRD | TmpScrolled
                    deriving (Eq, Enum, Show, Bounded)

scrollArea :: MonadIO m => ScrollAreaDef -> Widget -> Skin -> m (GuiWidget ScrollAreaData)
scrollArea ScrollAreaDef{..} parent skin = do
--    state <- newMonadIORef $ ScrollAreaStruct ScrollAreaNoFlags
        -- getChild = fmap fromJust . getWidgetChild selfWidget . fromEnum
    let initScrllArFns = noChildrenFns scrollAreaSize
    self@(GuiWidget selfWidget _) <- mkWidget scrollAreaFlags
                        (fromMaybe (formItemsMargin skin) $ formItemMargin scrollAreaItemDef)
                        ScrollAreaData parent initScrllArFns
    -- временно создаём widget скроллируемой области. Она должна быть в начале childs
    void $ mkSimpleWidget WidgetMarginNone selfWidget $ noChildrenFns scrollAreaSize

    let slidersColor = fromMaybe (GUI.scrollAreaSlidersColor skin) scrollAreaSlidersColor
        trackBarDef = def{ linearTrackBarFlags  = WidgetNoFlags
                         , linearTrackBarSliderDraw = \ _ _ r -> withBlendMode SDL.BlendAlphaBlend
                                (setColor slidersColor >> fillRect r)
                         }
    hBar  <- hLinearTrackBar trackBarDef selfWidget skin
    vBar  <- vLinearTrackBar trackBarDef selfWidget skin

    textureArrBtns <- runProxyCanvas parent $ getTexture "ScrollAreaArrBtns.png"
    let scrollW = scrollBarWidth skin
        arrBtnDef =       def{ buttonFormItemDef = def{formItemMargin= Just WidgetMarginNone}
                             , buttonSize = V2 scrollW scrollW
                             , buttonFlags = WidgetNoFlags
                             , buttonTexture = textureArrBtns
                             , buttonMouseInPictIx = Just 1
                             , buttonPressedPictIx = Just 1
                             }
    arrL  <- textureButton arrBtnDef selfWidget skin
    arrU  <- textureButton arrBtnDef{buttonInitPictRow = 1 } selfWidget skin
    arrRD <- textureButton arrBtnDef{buttonInitPictRow = 4 } selfWidget skin

    let getScrolledWidget :: MonadIO m => m Widget
        getScrolledWidget = fromJust <$> getWidgetChild selfWidget (fromEnum Scrolled)
        hSetCanvOff :: MonadIO m => Int -> m ()
        hSetCanvOff newV = do
            scrlW <- getScrolledWidget
            (SDL.Rectangle (P (V2 xC yC)) (V2 wC hC)) <- getWidgetCanvasRect scrlW
            when (newV/=xC) $ do
                setWidgetCanvasRect scrlW $ SDL.Rectangle (P (V2 newV yC)) (V2 wC hC)
                markWidgetForRedraw selfWidget
        hSetCanvOffRelative :: MonadIO m => Int -> m ()
        hSetCanvOffRelative relV = do
            scrlW <- getScrolledWidget
            (SDL.Rectangle (P (V2 xC yC)) (V2 wC hC)) <- getWidgetCanvasRect scrlW
            vmax <- getMaxValue hBar
            let newV = toBound 0 vmax $ xC + relV
            when (newV/=xC) $ do
                setWidgetCanvasRect scrlW $ SDL.Rectangle (P (V2 newV yC)) (V2 wC hC)
                updateByScrolled
                markWidgetForRedraw selfWidget
        vSetCanvOff :: MonadIO m => Int -> m ()
        vSetCanvOff newV = do
            scrlW <- getScrolledWidget
            (SDL.Rectangle (P (V2 xC yC)) (V2 wC hC)) <- getWidgetCanvasRect scrlW
            when (newV/=yC) $ do
                setWidgetCanvasRect scrlW $ SDL.Rectangle (P (V2 xC newV)) (V2 wC hC)
                markWidgetForRedraw selfWidget
        vSetCanvOffRelative :: MonadIO m => Int -> m ()
        vSetCanvOffRelative relV = do
            scrlW <- getScrolledWidget
            (SDL.Rectangle (P (V2 xC yC)) (V2 wC hC)) <- getWidgetCanvasRect scrlW
            vmax <- getMaxValue vBar
            let newV = toBound 0 vmax $ yC + relV
            when (newV/=yC) $ do
                setWidgetCanvasRect scrlW $ SDL.Rectangle (P (V2 xC newV)) (V2 wC hC)
                updateByScrolled
                markWidgetForRedraw selfWidget
        getBarsVisible :: MonadIO m => m (Bool,Bool)
        getBarsVisible = do
            hBarVisible <- allWidgetFlags (getWidget hBar) WidgetVisible
            vBarVisible <- allWidgetFlags (getWidget vBar) WidgetVisible
            return (hBarVisible,vBarVisible)
        arrRDSimpleUpdate  :: MonadIO m => Bool -> Bool -> m Bool
        arrRDSimpleUpdate False True = setRowNum arrRD 3 >> return True
        arrRDSimpleUpdate True False = setRowNum arrRD 2 >> return True
        arrRDSimpleUpdate _     _    = return False
        scrllArActiveFlags = WidgetVisible .|. WidgetEnable
        widgetOff   :: MonadIO m => GuiWidget a -> m ()
        widgetOff e = let w = getWidget e in
                      (widgetFlagsRemove w scrllArActiveFlags >> markWidgetForRedraw w)
        widgetOn   :: MonadIO m => GuiWidget a -> m ()
        widgetOn  e = let w = getWidget e in
                      (widgetFlagsAdd w scrllArActiveFlags >> markWidgetForRedraw w)
        updateByScrolled :: MonadIO m => m ()
        updateByScrolled = do
--          childrenCnt <- getWidgetChildrenCount selfWidget
--          when (childrenCnt == fromEnum (maxBound::ScrollAreaChilds)) $ do
            scrlW <- getScrolledWidget
            (SDL.Rectangle _ (V2 w h)) <- getWidgetRect scrlW
            (SDL.Rectangle (P (V2 xC yC)) (V2 wC hC)) <- getWidgetCanvasRect scrlW
            let needHBar = wC > w
                needVBar = hC > h
            (hBarVisible,vBarVisible) <- getBarsVisible
            if needHBar then do
                unless hBarVisible $ do
                    widgetOn hBar
                    widgetOn arrL
                setMaxValue hBar (wC-w)
                setValue hBar xC
            else when hBarVisible $ do
                    widgetOff hBar
                    widgetOff arrL
            if needVBar then do
                unless vBarVisible $ do
                    widgetOn vBar
                    widgetOn arrU
                setMaxValue vBar (hC-h)
                setValue vBar yC
            else when vBarVisible $ do
                    widgetOff vBar
                    widgetOff arrU
            if needHBar || needVBar then do
                unless (hBarVisible || vBarVisible) $
                    widgetOn arrRD
                updtd <- arrRDSimpleUpdate needHBar needVBar
                unless updtd $ do ms <- getMouseState arrRD
                                  when (ms == WidgetMouseOut) $ setRowNum arrRD 4
            else when vBarVisible $
                    widgetOff arrRD

        arrangeChild :: MonadIO m => GuiRect -> Int -> Widget -> m ()
        arrangeChild selfRect@(SDL.Rectangle (P (V2 selfX selfY)) (V2 selfW selfH)) ix child = do
            let childKind = toEnum ix
                r = case childKind of
                        Scrolled -> selfRect
                        HBar -> SDL.Rectangle (P (V2 (selfX+scrollW) (selfY+selfH-scrollW)))
                                                 (V2 (selfW-2*scrollW) scrollW)
                        VBar -> SDL.Rectangle (P (V2 (selfX+selfW-scrollW) (selfY+scrollW)))
                                                 (V2 scrollW (selfH-2*scrollW))
                        ArrL -> SDL.Rectangle (P (V2 selfX (selfY+selfH-scrollW))) (V2 scrollW scrollW)
                        ArrU -> SDL.Rectangle (P (V2 (selfX+selfW-scrollW) selfY)) (V2 scrollW scrollW)
                        ArrRD -> SDL.Rectangle (P (V2 (selfX+selfW-scrollW) (selfY+selfH-scrollW)))
                                                  (V2 scrollW scrollW)
                        TmpScrolled -> selfRect
--            liftIO $ putStrLn $ concat ["arrangeChild ",show childKind, "  selfRect=", rectToBriefStr selfRect,
--                        "  r=",rectToBriefStr r]
            widgetResizingIfChanged child r
        fnsCorrectionForTransparent :: MonadIO m => GuiWidget _a -> m ()
        fnsCorrectionForTransparent gw = do
            let widget' = getWidget gw
            fns <- getWidgetFns widget'
            setWidgetFns widget' fns{
                onGainedMouseFocus = \widget pnt -> onGainedMouseFocus fns widget pnt >>
                                                    markWidgetForRedraw selfWidget
                ,onLostMouseFocus = \widget -> onLostMouseFocus fns widget >> markWidgetForRedraw selfWidget
                                   }

    fnsCorrectionForTransparent hBar
    fnsCorrectionForTransparent vBar
    fnsCorrectionForTransparent arrL
    fnsCorrectionForTransparent arrU

    arrRDfns <- getWidgetFns $ getWidget arrRD
    setWidgetFns (getWidget arrRD) arrRDfns{
        onGainedMouseFocus = \widget p@(P (V2 x y)) -> do
            onGainedMouseFocus arrRDfns widget p
            (hBarVisible,vBarVisible) <- getBarsVisible
            updtd <- arrRDSimpleUpdate hBarVisible vBarVisible
            unless updtd $ setRowNum arrRD (if x < y then 2 else 3)
            markWidgetForRedraw selfWidget
        ,onLostMouseFocus = \widget -> do
            onLostMouseFocus arrRDfns widget
            (hBarVisible,vBarVisible) <- getBarsVisible
            updtd <- arrRDSimpleUpdate hBarVisible vBarVisible
            unless updtd $ setRowNum arrRD 4
            markWidgetForRedraw selfWidget
                                            }

    let scrllArFns = initScrllArFns{
        onSizeChangedParentNotiy = \widget child _sz -> do
            mbI <- getChildWidgetIx widget child
            whenIsJust mbI $ \i' -> do
                i <- if toEnum i' == TmpScrolled then do
                        lastChildReplaceFirst widget
                        fns <- getWidgetFns child
                        setWidgetFns child fns{
                            onMarkForRedrawNotiy = \widget' -> onMarkForRedrawNotiy fns widget' >>
                                                        markWidgetForRedraw selfWidget
                                   }
                        return 0
                     else return i'
                selfRect <- getWidgetCanvasRect widget
                arrangeChild selfRect i child
                when (i==0) updateByScrolled
        , onResizing = \widget newRect -> do
            void $ simpleOnResizing widget newRect
            selfRect <- getWidgetCanvasRect widget
            imapByWidgetChildren_  (arrangeChild selfRect) widget
            updateByScrolled
        , onMouseWheel = \widget (V2 _ dy) _ -> -- do
--            liftIO $ putStrLn "scrollArea onMouseWheel" -- $ concat
            when (dy /=0) $ do
                vSetCanvOffRelative $ negate dy * yV2 scrollAreaSteps
                markWidgetForRedraw widget
                                  }
    setWidgetFns selfWidget scrllArFns
    getWidgetRectWithMargin selfWidget >>= onResizing scrllArFns selfWidget

    onChanged hBar $  \ v -> do
--        liftIO $ putStrLn $ concat ["onChanged hBar ",show (v:: Int)]
        hSetCanvOff v
        -- markWidgetForRedraw selfWidget

    onChanged vBar $  \ v -> do
--        liftIO $ putStrLn $ concat ["onChanged vBar ",show (v:: Int)]
        vSetCanvOff v
        --  markWidgetForRedraw selfWidget

    onClick arrL $ do
--        liftIO $ putStrLn "onClick arrL"
        hSetCanvOffRelative $ negate $ xV2 $ scrollAreaSteps
        markWidgetForRedraw selfWidget

    onClick arrU $ do
--        liftIO $ putStrLn "onClick arrU"
        vSetCanvOffRelative $ negate $ yV2 $ scrollAreaSteps
        markWidgetForRedraw selfWidget

    onClick arrRD $ do
--        liftIO $ putStrLn "onClick arrRD"
        row <- getRowNum arrRD
        case row of
            2 -> hSetCanvOffRelative $ xV2 $ scrollAreaSteps
            3 -> vSetCanvOffRelative $ yV2 $ scrollAreaSteps
            _ -> return ()
        markWidgetForRedraw selfWidget

    return self

instance WidgetComposer (GuiWidget ScrollAreaData) where
   w $+ initF = createWidget (getWidget w) initF

