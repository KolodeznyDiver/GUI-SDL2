{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:      GUI.Widget.Container.ScrollArea
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <kolodeznydiver@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Виджет-контейнер в который можно поместить другой виджет для возможности прокрутки
-- его отображаемого изображения по вертикали и горизонтали.
-- Прокрутка scrollbar-ами, кнопками по их углам. По вертикали колесом мыши.
-- Вложенный виджет может "полагать" что его виртуальная область рисования имеет размеры @getWidgetCanvasRect@
-- Которую обычно сам и устанавливает, например :
--
-- >   onCreate = \widget -> setWidgetCanvasRect widget (SDL.Rectangle zero (V2 500 800)) >>
-- >                                       notifyParentAboutSize widget zero
-- > , onResizing = setWidgetRect
-- > , onDraw= \widget -> do
-- >               rect <- getWidgetCanvasRect widget
-- >     -- Здесь рисование в прямоугольнике, возможно большем чем реально отображаемая на экране область.
--
-- Левая верхняя точка CanvasRect, обычно в начале задаётся нулевой и в дальнейшем означает смещение
-- реальной области рисования оносительно виртуальной.

module GUI.Widget.Container.ScrollArea(
    ScrollAreaDef(..),ScrollAreaData,scrollArea
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Bits
import Data.Default
import Control.Monad.Extra (whenJust)
import qualified SDL
import SDL.Vect
import GUI
import qualified GUI.BaseLayer.Primitives as P
import GUI.Widget.Button
import GUI.Widget.Handlers
import GUI.Widget.LinearTrackBar

{-
-- Возможные настроки для scrollArea в будущем.
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

-- | Параметры настройки @scrollArea@.
data ScrollAreaDef = ScrollAreaDef {
      scrollAreaItemDef  :: FormItemWidgetDef  -- ^ Общие настройки для всех виджетов для форм,
                                               -- в настоящий момент только margin's.
    , scrollAreaSize      :: GuiSize -- ^ Размер без полей.
    , scrollAreaFlags     :: WidgetFlags -- ^ Флаги базового виджета.
    , scrollAreaSlidersColor :: Maybe GuiColor -- ^ Цвета слайдеров правого и нижнего scrollbat-ов,
                                               -- либо из 'Skin'.
    , scrollAreaArrowsColor :: Maybe GuiColor -- ^ Цвета треугольнико-стрелок по углам @scrollArea@,
                                              -- либо из 'Skin'.
    , scrollAreaSteps :: GuiSize -- ^ На сколько пикселей смещать окно области отрисовки в виртуальном
                                 -- пространстве при нажатии на кнопки.
                                    }
--                                    deriving (Show)

instance Default ScrollAreaDef where
    def = ScrollAreaDef { scrollAreaItemDef = def
                        , scrollAreaSize = V2 (-1) (-1)
                        , scrollAreaFlags = WidgetVisible .|. WidgetEnable .|. WidgetMouseWheelControl
                        , scrollAreaSlidersColor = Nothing
                        , scrollAreaArrowsColor = Nothing
                        , scrollAreaSteps = V2 100 100
                        }
{-
-- Возможные настроки для scrollArea в будущем.
newtype ScrollAreaStruct = ScrollAreaStruct    { scrllArFlags :: ScrollAreaFlags
                                               }

newtype ScrollAreaData = ScrollAreaData { scrllAr :: IORef ScrollAreaStruct }
-}

-- | Тип созданного scrollArea в настоящий момент не имеет пренастраиваемых параметров.
-- Размеры и позицию области отображений, размеется можно изменитять по @setWidgetCanvasRect@
-- для базового виджета.
-- Обычно используется как  @GuiWidget ScrollAreaData@.
data ScrollAreaData = ScrollAreaData

-- | Не экспортируемый, внутренний тип дочерних виджетов @scrollArea@.
-- Соответсвует номерам позиций в векторе дочерних виджетов.
data ScrollAreaChilds = Scrolled -- ^ Собственно, прокручиваемый виджет.
                      | HBar -- ^ Горизонтальный scrollbar (внизу).
                      | VBar -- ^ Вертикальный scrollbar (справа).
                      | ArrL -- ^ Кнопка "влево", в левом нижнем углу.
                      | ArrU -- ^ Кнопка "вверх", в правом верхнем углу.
                      | ArrRD -- ^ Кнопка "вниз или право" в правом нижнем углу.
                      | TmpScrolled -- ^ Временная позиция прокручиваемого виджета после вставки.
                                    -- далее перемещается в начало вектора виджетов.
                      deriving (Eq, Enum, Show, Bounded)

-- | Функция создания виджета-контейнера прокручивамой области.
scrollArea :: MonadIO m => ScrollAreaDef ->  -- ^ Параметры виджета.
                           Widget -> -- ^ Будующий предок в дереве виджетов.
                           Skin -> -- ^ Skin.
                           m (GuiWidget ScrollAreaData)
scrollArea ScrollAreaDef{..} parent skin = do
--    state <- newMonadIORef $ ScrollAreaStruct ScrollAreaNoFlags
        -- getChild = fmap fromJust . getWidgetChild selfWidget . fromEnum
    let initScrllArFns = noChildrenFns scrollAreaSize
    self <- mkWidget scrollAreaFlags
                        (fromMaybe (formItemsMargin skin) $ formItemMargin scrollAreaItemDef)
                        ScrollAreaData parent initScrllArFns
    let selfWidget = baseWidget self
    -- временно создаём widget скроллируемой области. Она должна быть в начале childs.
    void $ mkSimpleWidget WidgetMarginNone selfWidget $ noChildrenFns scrollAreaSize

    let slidersColor = fromMaybe (GUI.scrollAreaSlidersColor skin) scrollAreaSlidersColor
        trackBarDef = def{ linearTrackBarFlags  = WidgetNoFlags
                         , linearTrackBarSliderDraw = \ _ _ r -> withBlendMode SDL.BlendAlphaBlend
                                (setColor slidersColor >> fillRect r)
                         }
    hBar  <- hLinearTrackBar trackBarDef selfWidget skin
    vBar  <- vLinearTrackBar trackBarDef selfWidget skin

    textureArrBtns <- runProxyCanvas parent $ getTexture "ArrBtns.png"
    scrollW <- ((`div` 2).xV2) <$> P.getTextureSize textureArrBtns
--    let scrollW = trackBarWidth skin
    let arrBtnDef =       def{ buttonFormItemDef = def{formItemMargin= Just WidgetMarginNone}
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
            (SDL.Rectangle (P (V2 xC yC)) szC) <- getWidgetCanvasRect scrlW
            when (newV/=xC) $ do
                setWidgetCanvasRect scrlW $ SDL.Rectangle (P (V2 newV yC)) szC
                markWidgetForRedraw selfWidget
        hSetCanvOffRelative :: MonadIO m => Int -> m ()
        hSetCanvOffRelative relV = do
            scrlW <- getScrolledWidget
            (SDL.Rectangle (P (V2 xC yC)) szC) <- getWidgetCanvasRect scrlW
            vmax <- getMaxValue hBar
            let newV = toBound 0 vmax $ xC + relV
            when (newV/=xC) $ do
                setWidgetCanvasRect scrlW $ SDL.Rectangle (P (V2 newV yC)) szC
                updateByScrolled
                markWidgetForRedraw selfWidget
        vSetCanvOff :: MonadIO m => Int -> m ()
        vSetCanvOff newV = do
            scrlW <- getScrolledWidget
            (SDL.Rectangle (P (V2 xC yC)) szC) <- getWidgetCanvasRect scrlW
            when (newV/=yC) $ do
                setWidgetCanvasRect scrlW $ SDL.Rectangle (P (V2 xC newV)) szC
                markWidgetForRedraw selfWidget
        vSetCanvOffRelative :: MonadIO m => Int -> m ()
        vSetCanvOffRelative relV = do
            scrlW <- getScrolledWidget
            (SDL.Rectangle (P (V2 xC yC)) szC) <- getWidgetCanvasRect scrlW
            vmax <- getMaxValue vBar
            let newV = toBound 0 vmax $ yC + relV
            when (newV/=yC) $ do
                setWidgetCanvasRect scrlW $ SDL.Rectangle (P (V2 xC newV)) szC
                updateByScrolled
                markWidgetForRedraw selfWidget
        getBarsVisible :: MonadIO m => m (Bool,Bool)
        getBarsVisible = do
            hBarVisible <- allWidgetFlags (baseWidget hBar) WidgetVisible
            vBarVisible <- allWidgetFlags (baseWidget vBar) WidgetVisible
            return (hBarVisible,vBarVisible)
        arrRDSimpleUpdate  :: MonadIO m => Bool -> Bool -> m Bool
        arrRDSimpleUpdate False True = setRowNum arrRD 3 >> return True
        arrRDSimpleUpdate True False = setRowNum arrRD 2 >> return True
        arrRDSimpleUpdate _     _    = return False
        scrllArActiveFlags = WidgetVisible .|. WidgetEnable
        widgetOff   :: MonadIO m => GuiWidget a -> m ()
        widgetOff e = let w = baseWidget e in
                      (widgetFlagsRemove w scrllArActiveFlags >> markWidgetForRedraw w)
        widgetOn   :: MonadIO m => GuiWidget a -> m ()
        widgetOn  e = let w = baseWidget e in
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
                setLinearTrackBarSliderLn hBar $ fromIntegral w / fromIntegral wC
                setValue hBar xC
            else when hBarVisible $ do
                    widgetOff hBar
                    widgetOff arrL
            if needVBar then do
                unless vBarVisible $ do
                    widgetOn vBar
                    widgetOn arrU
                setMaxValue vBar (hC-h)
                setLinearTrackBarSliderLn vBar $ fromIntegral h / fromIntegral hC
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
            else when (hBarVisible || vBarVisible) $
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

    fnsCorrectionForTransparent hBar
    fnsCorrectionForTransparent vBar
    fnsCorrectionForTransparent arrL
    fnsCorrectionForTransparent arrU

    arrRDfns <- getWidgetFns $ baseWidget arrRD
    setWidgetFns (baseWidget arrRD) arrRDfns{
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
        onSizeChangedParentNotify = \widget child _sz -> do
            mbI <- getChildWidgetIx widget child
            whenJust mbI $ \i' -> do
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

-- | Реализация вставки виджета в scrollArea.
instance WidgetComposer (GuiWidget ScrollAreaData) where
   w $+ initF = createWidget (baseWidget w) initF

