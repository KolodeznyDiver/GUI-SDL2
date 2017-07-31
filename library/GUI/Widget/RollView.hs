{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module:      GUI.Widget.RollView
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <kolodeznydiver@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Виджет отображения списка с произвольной отрисовкой, а так же вспомогательные типы и функции
-- конкретизирующие способ отрисовки.

module GUI.Widget.RollView(
    -- * Флаги rollView
    RollOpts,RollFlags,pattern RollNoFlags, pattern MultiSelectRollFlag,pattern UseOddColorRollFlag
    ,pattern DrawItemPrepareRollFlag,pattern EnterAsClickRollFlag,pattern MouseTrackingRollFlag
    -- * Типы и классы rollView
    ,RollViewDef(..),RollViewable(..),RollViewData
    -- * Виджет отображения списка с произвольной отрисовкой.
    ,rollView
    -- * Отображение упорядоченного контейнера с элементами поддерживающими 'TextShow'.
    ,RollViewTSPrepare,RollViewTS(..),RollViewText(..)
    -- * Вспомогательные функции для создания @instance ... RollViewable@ с только текстовым отображением элементов.
    --   Могут пторебоваться только для создания своего варианта отображения контейнера текстом.
    ,rollViewPrepareTextOnly,rollViewDrawItemTextOnly
    -- * Выпадающее окно с @rollView@.
    ,popupRollView
    ) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Bits
import Data.IORef
import qualified TextShow as TS
import qualified SDL
import SDL.Vect
import qualified SDL.Font as FNT
import SDL.Font (Font)
import Control.Monad.Extra (whenJust)
import Data.Default
import GUI
import GUI.Widget.Handlers
import GUI.Widget.Container.ScrollArea
import Data.Container.DirectAccess
import Data.Vector.Utils (modifySlice)
import GUI.BaseLayer.PopupWindow

-- | Тэг для битовых флагов настройки виджетов списков
data RollOpts

-- | Тип для битовых флагов состояния виджета
type RollFlags = Flags RollOpts

pattern RollNoFlags :: RollFlags
pattern MultiSelectRollFlag :: RollFlags
pattern UseOddColorRollFlag :: RollFlags
pattern DrawItemPrepareRollFlag :: RollFlags
pattern EnterAsClickRollFlag :: RollFlags
pattern MouseTrackingRollFlag :: RollFlags

                                    --  5432109876543210
pattern RollNoFlags          = (Flags 0x0000000000000000) :: RollFlags
pattern MultiSelectRollFlag  = (Flags 0x0000000000000001) :: RollFlags
pattern UseOddColorRollFlag  = (Flags 0x0000000000000002) :: RollFlags
pattern DrawItemPrepareRollFlag = (Flags 0x0000000000000004) :: RollFlags
pattern EnterAsClickRollFlag = (Flags 0x0000000000000008) :: RollFlags
pattern MouseTrackingRollFlag = (Flags 0x0000000000000010) :: RollFlags

pattern PaddingX :: Coord
pattern PaddingX = 5
pattern PaddingY :: Coord
pattern PaddingY = 3

-- | Начальные настройки виджета списка с пользовательской отрисовкой элементов
data RollViewDef = RollViewDef {
          rollViewFormItemDef :: FormItemWidgetDef -- ^ Общие настройки для всех виджетов для форм,
                                                   -- в настоящий момент только margin's.
        , rollViewSize       :: GuiSize -- ^ Размер без полей.
        , rollViewFlags      :: WidgetFlags -- ^ Флаги базового виджета.
        , rollViewRollFlags  :: RollFlags -- ^ Флаги виджета списка.
        , rollViewRowNum     :: Coord -- ^ Начальный текущий номер ряда (номер элемента).
                               }


instance Default RollViewDef where
    def = RollViewDef { rollViewFormItemDef = def
                      , rollViewSize = zero
                      , rollViewFlags = WidgetVisible .|. WidgetEnable .|. WidgetFocusable .|. WidgetTabbed
                      , rollViewRollFlags = UseOddColorRollFlag .|. DrawItemPrepareRollFlag
                      , rollViewRowNum = 0
                      }

-- | Задаёт rollView с произвольной отрисовкой элементов.
class RollViewable a p | a -> p where
    -- | Вызывается при создании @rollView@ используя @runProxyCanvas@.
    rollViewPrepare :: MonadIO m =>
                       Skin ->
                       RollFlags ->
                       a -> -- ^ Отображаемые данные (могут изменться во время существования Widget-а.
                       Canvas m (Coord,p) -- ^ Высота строки и подготовленные данные для отрисовки.
    -- | Вызывается для отрисовки каждого элемента (строки).
    rollViewDrawItem :: MonadIO m =>
                        Widget -> -- ^ @rollView@.
                        Skin ->
                        RollFlags ->
                        GuiRect -> -- ^ Координаты области для рисования.
                        DecoreState -> -- ^ Цвета фона и переднего плана.
                        Bool -> -- ^ WidgetEnable ?
                        Bool -> -- ^ WidgetFocused ?
                        Bool -> -- ^ Элемент маркированный.
                        Bool -> -- ^ Текущий ли это элемент.
                        p -> -- ^ Данные для отрисовки сформированные в @rollViewPrepare@.
                        a -> -- ^ Отображаемые данные.
                        Int -> -- ^ Номер элемента.
                        Canvas m ()
    -- | Возвращает текущее число элементов.
    rollViewGetCount :: MonadIO m =>
                        Widget -> -- ^ @rollView@.
                        a -> -- ^ Отображаемые данные.
                        m Int

-- | Не экспортируемый тип записи. Хранится по ссылке в 'RollViewData'.
data RollViewHandlers = RollViewHandlers    { rllVwOnMove :: forall m. MonadIO m => Int -> m ()
                                            , rllVwOnClk :: forall m. MonadIO m => m ()
                                            , rllVwOnDblClk :: forall m. MonadIO m => m ()
                                            }

-- | Не экспортируемый тип записи. Хранится по ссылке в 'RollViewData'.
data RollViewState = RollViewState { rllVwCur :: Int
                                   , rllVwMarkers :: VUM.IOVector Bool
                                   }


-- | Тип созданного виджета. Обычно используется как  @GuiWidget RollViewData@.
data RollViewData a = RollViewData { rllVwDat :: a
                                   , rllVwHndlrs :: IORef RollViewHandlers
                                   , rllVw :: IORef RollViewState
                                   }

-- | Установка и извлечение номера текущего ряда.
instance RowNumProperty (GuiWidget (RollViewData a)) where
    setRowNum w v = do
        let widget = getWidget w
            RollViewData{..} = getWidgetData w
        RollViewState{..} <- readMonadIORef rllVw
        when ( (v /= rllVwCur) && (v>=0) && (v < VUM.length rllVwMarkers)) $ do
            writeMonadIORef rllVw $ RollViewState v rllVwMarkers
            markWidgetForRedraw widget
    getRowNum w = rllVwCur <$> readMonadIORef (rllVw $ getWidgetData w)

-- | Установка функции-обработчика события изменение текущей позиции (номера ряда).
instance Moveable (GuiWidget (RollViewData a)) Int where
    onMove w a = modifyMonadIORef' (rllVwHndlrs $ getWidgetData w) (\d -> d{rllVwOnMove= a})

-- | Установка функции-обработчика одинарного щелчка.
instance Clickable (GuiWidget (RollViewData a)) where
    onClick w a = modifyMonadIORef' (rllVwHndlrs $ getWidgetData w) (\d -> d{rllVwOnClk= a})

-- | Установка функции-обработчика двойного щелчка.
instance DoubleClickable (GuiWidget (RollViewData a)) where
    onDoubleClick w a = modifyMonadIORef' (rllVwHndlrs $ getWidgetData w) (\d -> d{rllVwOnDblClk= a})

-- | Установка и извлечение @VU.Vectoor Bool@ - означающего какие элементы (ряды) маркированы.
-- Для установки значений маркеров вектор должен быть той же длины что и вектор в 'RollViewData'.
instance MarkersProperty (GuiWidget (RollViewData a)) where
    setMarkers w n = do
        v <- rllVwMarkers <$> readMonadIORef (rllVw $ getWidgetData w)
        when (VUM.length v == VU.length n) $ do
            liftIO $ VU.imapM_ (VUM.write v) n
            markWidgetForRedraw $ getWidget w
    getMarkers w = do
        v <- rllVwMarkers <$> readMonadIORef (rllVw $ getWidgetData w)
        liftIO $ VU.generateM (VUM.length v) (VUM.read v)

-- | Функция создания виджета списка.
rollView :: (MonadIO m, RollViewable a b) =>
                         RollViewDef -> -- ^ Параметры виджета.
                         a -> -- ^ Исходные отображаемые данные.
                         Widget -> -- ^ Будующий предок в дереве виджетов.
                         Skin -> -- ^ Skin.
                         m (GuiWidget (RollViewData a))
rollView RollViewDef{..} a parent skin = do
    (itemH,p) <- runProxyCanvas parent $ rollViewPrepare skin rollViewRollFlags a
    rfH <- newMonadIORef $ RollViewHandlers (\_ -> return ()) (return ()) (return ())
    rfSt <- newMonadIORef =<< (RollViewState rollViewRowNum <$> liftIO (VUM.new 0))
    scrll <- scrollArea def{ scrollAreaItemDef = rollViewFormItemDef
                           , scrollAreaSize = rollViewSize }
                        parent skin
    let fns = noChildrenFns rollViewSize
        (V2 width heigth) = rollViewSize
        isMultiSelect = (rollViewRollFlags .&. MultiSelectRollFlag) /= RollNoFlags
        toRange cnt = toBound 0 (cnt-1)
        doOnMove old new = when (old /= new)
                             (readMonadIORef rfH >>= ( ( $ new) . rllVwOnMove))

        cntUpdt :: MonadIO m => Widget -> m (RollViewState,Int)
        cntUpdt widget = do
            RollViewState{..} <- readMonadIORef rfSt
            newCnt <- rollViewGetCount widget a
            let iCur = toRange newCnt rllVwCur
                cnt = VUM.length rllVwMarkers
                ret s = do  let r = RollViewState iCur s
                            writeMonadIORef rfSt r
                            return (r,newCnt)
            r <- if | newCnt < cnt -> ret $ VUM.take newCnt rllVwMarkers
                    | newCnt > cnt -> do
                        let dlt = newCnt - cnt
                        s <- liftIO $ VUM.unsafeGrow rllVwMarkers dlt
                        liftIO $ VUM.set (VUM.unsafeSlice cnt dlt s) False
                        ret s
                    | otherwise -> return (RollViewState iCur rllVwMarkers,newCnt)
            doOnMove rllVwCur iCur
            return r
        getCoords :: MonadIO m => Widget -> m (GuiSize,GuiRect)
        getCoords widget = do
            (SDL.Rectangle _ widgSz) <- getWidgetRect widget
            cr <- getWidgetCanvasRect widget
            return (widgSz,cr)
        updt :: MonadIO m => Widget -> m (RollViewState,Int,GuiSize,GuiRect)
        updt widget = do
            (rollViewState,cnt) <- cntUpdt widget
            (widgSz@(V2 _ widgH),cr) <- getCoords widget
            let cr' = recalcCanvasRect cnt widgH cr
            when (cr /= cr')
                (setWidgetCanvasRect widget cr' >> scrollNotify widget)
            return (rollViewState,cnt,widgSz,cr')
        -- Пересчёт при изменении кол-ва элементов
        recalcCanvasRect :: Int -> Coord -> GuiRect -> GuiRect
        recalcCanvasRect cnt widgH (SDL.Rectangle (P (V2 xC yC)) (V2 wC _hC)) =
            let h = cnt*itemH
                y = max 0 $ min yC $ h - widgH in
            SDL.Rectangle (P (V2 xC y)) (V2 wC h)
        -- Пересчёт при изменении текущей позиции
        arrangeCanvasRect :: Int -> Coord -> GuiRect -> GuiRect
        arrangeCanvasRect pos widgH (SDL.Rectangle (P (V2 xC yC)) szC) =
            let y = pos * itemH in
            SDL.Rectangle (P (V2 xC
                (if | y < yC -> y
                    | (y+itemH) > (yC + widgH) -> max 0 (y + itemH - widgH)
                    | otherwise -> yC
                ))) szC
        coord2ix cnt (SDL.Rectangle _{-(P (V2 _xC yC))-} (V2 _wC hC)) y =
--            let  y' = y + yC in
            if y>= hC then (-1)
            else let i= y `div` itemH in
                 if i>= cnt then (-1) else i
        scrollNotify :: MonadIO m => Widget -> m ()
        scrollNotify widget = notifyParentAboutSize widget =<< (sizeOfRect <$> getWidgetRect widget)
        calcFromAndLn i j = if i < j then (i,j-i+1) else (j,i-j+1)

        doMove :: MonadIO m => Widget ->
                               ShiftCtrlAlt ->
                               ( Int -> -- cnt
                                 Int -> -- old pos
                                 Coord -> -- widgH
                                 GuiRect -> -- canvas rect
                                 Maybe (Int  -- new pos
                                       ,Int) -- Маркировать от
                               ) -> m (Maybe Int)
        doMove widget ShiftCtrlAlt{isShift=isS,isCtrl=isC} f = do
            (RollViewState{..},cnt,V2 _widgW widgH,cr) <- updt widget
            case f cnt rllVwCur widgH cr of
              Just (newPos,markFrom) -> do
                let newPos' = toRange cnt newPos
                when (isMultiSelect && (cnt>0)) $ liftIO $ do
                    unless (isC || isS) $
                        VUM.set rllVwMarkers False
                    when (isC || isS) $
                        let markFrom' = toRange cnt markFrom
                            (iFrom,iLn) = calcFromAndLn newPos' markFrom'
                        in if isC then modifySlice rllVwMarkers iFrom iLn not
                           else VUM.set (VUM.unsafeSlice iFrom iLn rllVwMarkers) True
                writeMonadIORef rfSt $ RollViewState newPos' rllVwMarkers
                let cr' = arrangeCanvasRect newPos' widgH cr
                when (cr /= cr')
                    (setWidgetCanvasRect widget cr' >> scrollNotify widget)
                markWidgetForRedraw widget
                doOnMove rllVwCur newPos'
                return $ Just newPos'
              _ -> return Nothing

        doMouseMove :: MonadIO m => Widget -> ShiftCtrlAlt -> Coord -> m (Maybe Int)
        doMouseMove widget shiftCtrlAlt y = doMove widget shiftCtrlAlt $ \ cnt _pos _widgH cr ->
              let i = coord2ix cnt cr y in
              if i<0 then Nothing
              else Just (i,i)


    mkWidget rollViewFlags WidgetMarginNone (RollViewData a rfH rfSt)
                        (getWidget scrll) fns{
       onCreate = \widget -> do
            (_,cnt) <- cntUpdt widget
            let cr = recalcCanvasRect cnt heigth (SDL.Rectangle zero (V2 width 0))
            setWidgetCanvasRect widget cr >> notifyParentAboutSize widget zero
--            liftIO $ putStrLn $ concat ["itemH=",show itemH]
            -- onCreate fns widget

       ,onResizing= \widget newRect -> do
            void $ setWidgetRectWithMarginShrink widget newRect
            getWidgetParent widget >>=  markWidgetForRedraw
            (_,cnt,V2 _ widgH,cr) <- updt widget
            setWidgetCanvasRect widget $ recalcCanvasRect cnt widgH cr

       ,onMouseMotion = \widget btnsLst (P (V2 _ y)) _{-(V2 _ relMvY)-} ->
           if | SDL.ButtonLeft `elem` btnsLst -> do
                shiftCtrlAlt <- getActualShiftCtrlAlt
                when (isCtrl shiftCtrlAlt || isShift shiftCtrlAlt) $
                    void $ doMove widget shiftCtrlAlt $ \ cnt pos _widgH cr ->
                        let i' = coord2ix cnt cr y
                            i = if i' < 0 then cnt - 1
                                else i'
                        in Just (pos,i)
              | ((rollViewRollFlags .&. MouseTrackingRollFlag) /= RollNoFlags) -> do
                    shiftCtrlAlt <- getActualShiftCtrlAlt
                    when (shiftCtrlAlt == ShiftCtrlAlt False False False) $
                        void $ doMouseMove widget shiftCtrlAlt y
              | otherwise -> return ()
       ,onMouseButton = \widget motion mouseButton clicks (P (V2 _ y)) ->
           when ((motion==SDL.Pressed) && (mouseButton == SDL.ButtonLeft)) $ do
                setWidgetFocus widget
                shiftCtrlAlt <- getActualShiftCtrlAlt
                mb <- doMouseMove widget shiftCtrlAlt y
                whenJust mb $ \_ ->
                    join $ (if clicks==1 then rllVwOnClk else rllVwOnDblClk) <$> readMonadIORef rfH

       ,onKeyboard = \widget motion _repeated keycode km -> when (motion==SDL.Pressed) $ do
            let shiftCtrlAlt@ShiftCtrlAlt{isShift=isS,isCtrl=isC,isAlt=isA} = getShftCtrlAlt km
            if isEnterKey keycode && shiftCtrlAlt == ShiftCtrlAlt False False False then
                when ((rollViewRollFlags .&. EnterAsClickRollFlag) /= RollNoFlags) $
                    join $ rllVwOnClk <$> readMonadIORef rfH
            else case keycode of
                SDL.KeycodeA | not isS && isC && not isA && isMultiSelect -> do
                    (RollViewState{..},_cnt) <- cntUpdt widget
                    liftIO $ VUM.set rllVwMarkers True
                    markWidgetForRedraw widget
                SDL.KeycodeHome | not isC && not isA -> void $ doMove widget shiftCtrlAlt $
                    \ _cnt pos _widgH _cr -> Just (0,pos)
                SDL.KeycodeEnd | not isC && not isA -> void $ doMove widget shiftCtrlAlt $
                    \ cnt pos _widgH _cr -> Just (cnt-1,pos)
                SDL.KeycodeUp | not isC && not isA -> void $ doMove widget shiftCtrlAlt $
                    \ _cnt pos _widgH _cr -> Just (pos-1,pos)
                SDL.KeycodeDown | not isC && not isA -> void $ doMove widget shiftCtrlAlt $
                    \ _cnt pos _widgH _cr -> Just (pos+1,pos)
                SDL.KeycodePageUp | not isC && not isA -> void $ doMove widget shiftCtrlAlt $
                    \ _cnt pos widgH _cr -> Just (pos - (widgH `div` itemH) ,pos)
                SDL.KeycodePageDown | not isC && not isA -> void $ doMove widget shiftCtrlAlt $
                    \ _cnt pos widgH _cr -> Just (pos + (widgH `div` itemH) ,pos)
                _ -> return ()
       ,onDraw= \widget -> do
            fl <- getWidgetFlags widget
            (RollViewState{..},cnt,widgSz@(V2 widgW widgH), SDL.Rectangle pC@(P (V2 _xC yC)) _) <- updt widget
--            liftIO $ putStrLn $ concat ["cnt=",show cnt," widgSz=",show widgSz," yC=",show yC]
--            sPrnt <- getWidgetParent widget >>= widgetCoordsToStr
--            sWdg <- widgetCoordsToStr widget
--            liftIO $ putStrLn sWdg
--            liftIO $ putStrLn $ concat ["parent : ",sPrnt,"\n",sWdg]
            let widgR = SDL.Rectangle pC widgSz
                ena = (fl .&. WidgetEnable) /= WidgetNoFlags
                isFocused = (fl .&. WidgetFocused) /= WidgetNoFlags
                itemDraw i | i >= cnt = return ()
                           | otherwise = let y = i*itemH in
                                         when (y < (yC + widgH)) $ do
                                let r= SDL.Rectangle (P (V2 0 y)) (V2 widgW itemH)
                                    r'= rectIntersection widgR r
                                    isCur = i == rllVwCur
                                isMarked <- if not ena || not isMultiSelect
                                            then return False
                                            else liftIO $ VUM.read rllVwMarkers i
                                let ds = DecoreState
                                            ((if | ena && (isMarked || isCur) -> decoreBkColor . selectedDecore
                                                 | ena && ((rollViewRollFlags .&. UseOddColorRollFlag) /= RollNoFlags)
                                                       && odd i -> oddBkColor
                                                 | otherwise -> decoreBkColor . windowDecore) skin)
                                            ((if | not ena -> windowDisabledFgColor
                                                 | isMarked || isCur -> decoreFgColor . selectedDecore
                                                 | otherwise -> decoreFgColor . windowDecore) skin)
--                                liftIO $ putStrLn $ concat ["i=",show i," y=",show y," r=",rectToBriefStr r,
--                                    " r'=",rectToBriefStr r']
                                when ( (rollViewRollFlags .&. DrawItemPrepareRollFlag) /= RollNoFlags) $ do
                                    setColor $ decoreBkColor ds
                                    fillRect r'
                                    when (ena && isCur && isFocused) $ do
                                        setColor $ decoreFgColor ds
                                        drawRect r -- $ SDL.Rectangle (P (V2 0 y)) (V2 (widgW-1) (itemH-1))
                                --withClipRect r' $
                                rollViewDrawItem widget skin rollViewRollFlags r ds
                                        ena isFocused isMarked isCur p a i
                                itemDraw $ i + 1
            setColor $ decoreBkColor (windowDecore skin)
            fillRect widgR
            itemDraw $ yC `div` itemH
--            setColor $ rgb 255 0 0
--            drawRect $ shrinkRect' 2 widgR
                                    }

-- | Подготовленные данные для отрисовки в  @rollView@  упорядоченного контейнера
--   с элементами поддерживающими 'TextShow'.
newtype RollViewTSPrepare = RollViewTSPrepare Font

-- | Обёртка для использования с @rollView@ упорядоченного контейнера с элементами поддерживающими 'TextShow'.
newtype RollViewTS c v = RollViewTS c

-- | Обёртка для использования с @rollView@ упорядоченного контейнера с элементами типа 'Text'.
newtype RollViewText c = RollViewText c

-- | Вспомогательная функция для создания @instance ... RollViewable@ с только текстовым отображением элементов.
rollViewPrepareTextOnly :: MonadIO m => Canvas m (Coord,Font)
rollViewPrepareTextOnly = do
    fnt <- getFont "roll"
    fntHeight <- FNT.lineSkip fnt -- FNT.height fnt
    return (fntHeight + 2 * PaddingY,fnt)

-- | Вспомогательная функция для создания @instance ... RollViewable@ с отображением элементов одной строкой.
rollViewDrawItemTextOnly :: (MonadIO m, DAROContainer c v, TS.TextShow v) =>
                        Font -> -- ^ Шрифт.
                        GuiRect -> -- ^ Координаты области для рисования.
                        DecoreState -> -- ^ Цвета фона и переднего плана.
                        c -> -- ^ Контейнер с данными.
                        Int -> -- ^ Номер элемента.
                        (v -> T.Text) -> -- ^ Функция преобразования элемента контейнера в 'Text'.
                        Canvas m ()
rollViewDrawItemTextOnly fnt rect decoreState c ix f =
    drawTextAligned fnt AlignLeftCenter (decoreFgColor decoreState) (DrawStrOpaque (decoreBkColor decoreState))
        (shrinkRect (V2 PaddingX PaddingY) rect) . f =<< getItemDARO c ix

instance (DAROContainer c v, TS.TextShow v) => RollViewable (RollViewTS c v) RollViewTSPrepare where
    rollViewPrepare _skin _rollFlags _container = fmap RollViewTSPrepare <$> rollViewPrepareTextOnly

    -- | Вызывается для отрисовки каждого элемента (строки).
    rollViewDrawItem _widget _skin _rollFlags rect decoreState
                     _isEna _isFocused _isMarked _isCur (RollViewTSPrepare fnt) (RollViewTS c) ix =
        rollViewDrawItemTextOnly fnt rect decoreState c ix TS.showt

    -- | Возвращает текущее число элементов.
    rollViewGetCount _widget (RollViewTS c) = sizeDARO c

instance DAROContainer c T.Text => RollViewable (RollViewText c) RollViewTSPrepare where
    rollViewPrepare _skin _rollFlags _container = fmap RollViewTSPrepare <$> rollViewPrepareTextOnly

    rollViewDrawItem _widget _skin _rollFlags rect decoreState
                     _isEna _isFocused _isMarked _isCur (RollViewTSPrepare fnt) (RollViewText c) ix =
        rollViewDrawItemTextOnly fnt rect decoreState c ix id

    rollViewGetCount _widget (RollViewText c) = sizeDARO c


-- | Функция создающая окно с @rollView@.
popupRollView :: (MonadIO m, RollViewable a b) =>
                -- | Виджет активного сейчас окна. Popup окно станет дочерним по отношению к этому окну.
                Widget ->
                -- | Начальные координаты окна в координатах указанного виджета.
                GuiRect ->
                a -> -- ^ Исходные отображаемые данные.
                (forall n. MonadIO n => Int -> n ()) ->  -- ^ Функция вызываемая при выборе пункта меню
                                                         -- при закрытии окна.
                m ()
popupRollView parent rect a f = do
    !winSDL <- getSDLWindow =<< getWidgetWindow parent
    win <- mkPopupWindow parent rect
    roll <- win $+ rollView def { rollViewFormItemDef = def{formItemMargin=Just WidgetMarginNone}
                                , rollViewSize = sizeOfRect rect
                                , rollViewRollFlags = rollViewRollFlags def .|.
                                    EnterAsClickRollFlag .|. MouseTrackingRollFlag
                                } a
    onClick roll $ do
        !i <- getRowNum roll
--        delWindow win
        SDL.showWindow winSDL >> SDL.raiseWindow winSDL
        f i
    setFocus roll