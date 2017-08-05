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
-- Module:      GUI.Widget.ListView
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <kolodeznydiver@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Виджет отображения списка с произвольной отрисовкой, а так же вспомогательные типы и функции
-- конкретизирующие способ отрисовки.

module GUI.Widget.ListView(
    -- * Флаги listView
    ListFlagTag,ListViewFlags,pattern ListViewNoFlags, pattern MultiSelectListViewFlag
    ,pattern UseOddColorListViewFlag
    ,pattern DrawItemPrepareListViewFlag,pattern EnterAsClickListViewFlag,pattern MouseTrackingListViewFlag
    -- * Типы и классы listView
    ,ListViewDef(..),ListViewable(..),ListViewData
    -- * Виджет отображения списка с произвольной отрисовкой.
    ,listView
    -- * Отображение упорядоченного контейнера с элементами поддерживающими 'TextShow'.
    ,ListViewTSPrepare,ListViewTS(..),ListViewText(..)
    -- * Вспомогательные функции для создания @instance ... ListViewable@ с только текстовым отображением элементов.
    --   Могут пторебоваться только для создания своего варианта отображения контейнера текстом.
    ,listViewPrepareTextOnly,listViewDrawItemTextOnly
    -- * Выпадающее окно с @listView@.
    ,popupListView
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
data ListFlagTag

-- | Тип для битовых флагов состояния виджета
type ListViewFlags = Flags ListFlagTag

pattern ListViewNoFlags :: ListViewFlags
pattern MultiSelectListViewFlag :: ListViewFlags
pattern UseOddColorListViewFlag :: ListViewFlags
pattern DrawItemPrepareListViewFlag :: ListViewFlags
pattern EnterAsClickListViewFlag :: ListViewFlags
pattern MouseTrackingListViewFlag :: ListViewFlags

                                    --  5432109876543210
pattern ListViewNoFlags          = (Flags 0x0000000000000000) :: ListViewFlags
pattern MultiSelectListViewFlag  = (Flags 0x0000000000000001) :: ListViewFlags
pattern UseOddColorListViewFlag  = (Flags 0x0000000000000002) :: ListViewFlags
pattern DrawItemPrepareListViewFlag = (Flags 0x0000000000000004) :: ListViewFlags
pattern EnterAsClickListViewFlag = (Flags 0x0000000000000008) :: ListViewFlags
pattern MouseTrackingListViewFlag = (Flags 0x0000000000000010) :: ListViewFlags

pattern PaddingX :: Coord
pattern PaddingX = 5
pattern PaddingY :: Coord
pattern PaddingY = 3

-- | Начальные настройки виджета списка с пользовательской отрисовкой элементов
data ListViewDef = ListViewDef {
          listViewFormItemDef :: FormItemWidgetDef -- ^ Общие настройки для всех виджетов для форм,
                                                   -- в настоящий момент только margin's.
        , listViewSize       :: GuiSize -- ^ Размер без полей.
        , listViewFlags      :: WidgetFlags -- ^ Флаги базового виджета.
        , listViewListViewFlags  :: ListViewFlags -- ^ Флаги виджета списка.
        , listViewIx     :: Int -- ^ Начальный текущий номер ряда (номер элемента).
                               }


instance Default ListViewDef where
    def = ListViewDef { listViewFormItemDef = def
                      , listViewSize = zero
                      , listViewFlags = WidgetVisible .|. WidgetEnable .|. WidgetFocusable .|. WidgetTabbed
                      , listViewListViewFlags = UseOddColorListViewFlag .|. DrawItemPrepareListViewFlag
                      , listViewIx = 0
                      }

-- | Задаёт listView с произвольной отрисовкой элементов.
class ListViewable a p | a -> p where
    -- | Вызывается при создании @listView@ используя @runProxyCanvas@.
    listViewPrepare :: MonadIO m =>
                       Skin ->
                       ListViewFlags ->
                       a -> -- ^ Отображаемые данные (могут изменться во время существования Widget-а.
                       Canvas m (Coord,p) -- ^ Высота строки и подготовленные данные для отрисовки.
    -- | Вызывается для отрисовки каждого элемента (строки).
    listViewDrawItem :: MonadIO m =>
                        Widget -> -- ^ @listView@.
                        Skin ->
                        ListViewFlags ->
                        GuiRect -> -- ^ Координаты области для рисования.
                        DecoreState -> -- ^ Цвета фона и переднего плана.
                        Bool -> -- ^ WidgetEnable ?
                        Bool -> -- ^ WidgetFocused ?
                        Bool -> -- ^ Элемент маркированный.
                        Bool -> -- ^ Текущий ли это элемент.
                        p -> -- ^ Данные для отрисовки сформированные в @listViewPrepare@.
                        a -> -- ^ Отображаемые данные.
                        Int -> -- ^ Номер элемента.
                        Canvas m ()
    -- | Возвращает текущее число элементов.
    listViewGetCount :: MonadIO m =>
                        Widget -> -- ^ @listView@.
                        a -> -- ^ Отображаемые данные.
                        m Int

-- | Не экспортируемый тип записи. Хранится по ссылке в 'ListViewData'.
data ListViewHandlers = ListViewHandlers    { lstVwOnMove :: forall m. MonadIO m => Int -> m ()
                                            , lstVwOnClk :: forall m. MonadIO m => m ()
                                            , lstVwOnDblClk :: forall m. MonadIO m => m ()
                                            }

-- | Не экспортируемый тип записи. Хранится по ссылке в 'ListViewData'.
data ListViewState = ListViewState { lstVwCur :: Int
                                   , lstVwMarkers :: VUM.IOVector Bool
                                   }


-- | Тип созданного виджета. Обычно используется как  @GuiWidget ListViewData@.
data ListViewData a = ListViewData { lstVwDat :: a
                                   , lstVwHndlrs :: IORef ListViewHandlers
                                   , lstVw :: IORef ListViewState
                                   }

-- | Установка и извлечение номера текущего элемента.
instance IxProperty (GuiWidget (ListViewData a)) where
    setIx w v = do
        let widget = getWidget w
            ListViewData{..} = getWidgetData w
        ListViewState{..} <- readMonadIORef lstVw
        when ( (v /= lstVwCur) && (v>=0) && (v < VUM.length lstVwMarkers)) $ do
            writeMonadIORef lstVw $ ListViewState v lstVwMarkers
            readMonadIORef lstVwHndlrs >>= ( ( $ v) . lstVwOnMove)
            markWidgetForRedraw widget
    getIx w = lstVwCur <$> readMonadIORef (lstVw $ getWidgetData w)

-- | Установка функции-обработчика события изменение текущей позиции (номера ряда).
instance Moveable (GuiWidget (ListViewData a)) Int where
    onMove w a = modifyMonadIORef' (lstVwHndlrs $ getWidgetData w) (\d -> d{lstVwOnMove= a})

-- | Установка функции-обработчика одинарного щелчка.
instance Clickable (GuiWidget (ListViewData a)) where
    onClick w a = modifyMonadIORef' (lstVwHndlrs $ getWidgetData w) (\d -> d{lstVwOnClk= a})

-- | Установка функции-обработчика двойного щелчка.
instance DoubleClickable (GuiWidget (ListViewData a)) where
    onDoubleClick w a = modifyMonadIORef' (lstVwHndlrs $ getWidgetData w) (\d -> d{lstVwOnDblClk= a})

-- | Установка и извлечение @VU.Vectoor Bool@ - означающего какие элементы (ряды) маркированы.
-- Для установки значений маркеров вектор должен быть той же длины что и вектор в 'ListViewData'.
instance MarkersProperty (GuiWidget (ListViewData a)) where
    setMarkers w n = do
        v <- lstVwMarkers <$> readMonadIORef (lstVw $ getWidgetData w)
        when (VUM.length v == VU.length n) $ do
            liftIO $ VU.imapM_ (VUM.write v) n
            markWidgetForRedraw $ getWidget w
    getMarkers w = do
        v <- lstVwMarkers <$> readMonadIORef (lstVw $ getWidgetData w)
        liftIO $ VU.generateM (VUM.length v) (VUM.read v)

-- | Функция создания виджета списка.
listView :: (MonadIO m, ListViewable a _p) =>
                         ListViewDef -> -- ^ Параметры виджета.
                         a -> -- ^ Исходные отображаемые данные.
                         Widget -> -- ^ Будующий предок в дереве виджетов.
                         Skin -> -- ^ Skin.
                         m (GuiWidget (ListViewData a))
listView ListViewDef{..} a parent skin = do
    (itemH,p) <- runProxyCanvas parent $ listViewPrepare skin listViewListViewFlags a
    rfH <- newMonadIORef $ ListViewHandlers (\_ -> return ()) (return ()) (return ())
    rfSt <- newMonadIORef =<< (ListViewState listViewIx <$> liftIO (VUM.new 0))
    scrll <- scrollArea def{ scrollAreaItemDef = listViewFormItemDef
                           , scrollAreaSize = listViewSize }
                        parent skin
    let fns = noChildrenFns listViewSize
        (V2 width heigth) = listViewSize
        isMultiSelect = (listViewListViewFlags .&. MultiSelectListViewFlag) /= ListViewNoFlags
        toRange cnt = toBound 0 (cnt-1)
        doOnMove old new = when (old /= new)
                             (readMonadIORef rfH >>= ( ( $ new) . lstVwOnMove))

        cntUpdt :: MonadIO m => Widget -> m (ListViewState,Int)
        cntUpdt widget = do
            ListViewState{..} <- readMonadIORef rfSt
            newCnt <- listViewGetCount widget a
            let iCur = toRange newCnt lstVwCur
                cnt = VUM.length lstVwMarkers
                ret s = do  let r = ListViewState iCur s
                            writeMonadIORef rfSt r
                            return (r,newCnt)
            r <- if | newCnt < cnt -> ret $ VUM.take newCnt lstVwMarkers
                    | newCnt > cnt -> do
                        let dlt = newCnt - cnt
                        s <- liftIO $ VUM.unsafeGrow lstVwMarkers dlt
                        liftIO $ VUM.set (VUM.unsafeSlice cnt dlt s) False
                        ret s
                    | otherwise -> return (ListViewState iCur lstVwMarkers,newCnt)
            doOnMove lstVwCur iCur
            return r
        getCoords :: MonadIO m => Widget -> m (GuiSize,GuiRect)
        getCoords widget = do
            (SDL.Rectangle _ widgSz) <- getWidgetRect widget
            cr <- getWidgetCanvasRect widget
            return (widgSz,cr)
        updt :: MonadIO m => Widget -> m (ListViewState,Int,GuiSize,GuiRect)
        updt widget = do
            (listViewState,cnt) <- cntUpdt widget
            (widgSz@(V2 _ widgH),cr) <- getCoords widget
            let cr' = recalcCanvasRect cnt widgH cr
            when (cr /= cr')
                (setWidgetCanvasRect widget cr' >> scrollNotify widget)
            return (listViewState,cnt,widgSz,cr')
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
            (ListViewState{..},cnt,V2 _widgW widgH,cr) <- updt widget
            case f cnt lstVwCur widgH cr of
              Just (newPos,markFrom) -> do
                let newPos' = toRange cnt newPos
                when (isMultiSelect && (cnt>0)) $ liftIO $ do
                    unless (isC || isS) $
                        VUM.set lstVwMarkers False
                    when (isC || isS) $
                        let markFrom' = toRange cnt markFrom
                            (iFrom,iLn) = calcFromAndLn newPos' markFrom'
                        in if isC then modifySlice lstVwMarkers iFrom iLn not
                           else VUM.set (VUM.unsafeSlice iFrom iLn lstVwMarkers) True
                writeMonadIORef rfSt $ ListViewState newPos' lstVwMarkers
                let cr' = arrangeCanvasRect newPos' widgH cr
                when (cr /= cr')
                    (setWidgetCanvasRect widget cr' >> scrollNotify widget)
                markWidgetForRedraw widget
                doOnMove lstVwCur newPos'
                return $ Just newPos'
              _ -> return Nothing

        doMouseMove :: MonadIO m => Widget -> ShiftCtrlAlt -> Coord -> m (Maybe Int)
        doMouseMove widget shiftCtrlAlt y = doMove widget shiftCtrlAlt $ \ cnt _pos _widgH cr ->
              let i = coord2ix cnt cr y in
              if i<0 then Nothing
              else Just (i,i)


    mkWidget listViewFlags WidgetMarginNone (ListViewData a rfH rfSt)
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
              | ((listViewListViewFlags .&. MouseTrackingListViewFlag) /= ListViewNoFlags) -> do
                    shiftCtrlAlt <- getActualShiftCtrlAlt
                    when (shiftCtrlAlt == ShiftCtrlAlt False False False) $
                        void $ doMouseMove widget shiftCtrlAlt y
              | otherwise -> return ()
       ,onMouseButton = \widget motion mouseButton clicks (P (V2 _ y)) ->
           when ((motion==SDL.Pressed) && (mouseButton == SDL.ButtonLeft)) $ do
--                setWidgetFocus widget
                shiftCtrlAlt <- getActualShiftCtrlAlt
                mb <- doMouseMove widget shiftCtrlAlt y
                whenJust mb $ \_ ->
                    join $ (if clicks==1 then lstVwOnClk else lstVwOnDblClk) <$> readMonadIORef rfH

       ,onKeyboard = \widget motion _repeated keycode km -> when (motion==SDL.Pressed) $ do
            let shiftCtrlAlt@ShiftCtrlAlt{isShift=isS,isCtrl=isC,isAlt=isA} = getShftCtrlAlt km
            if isEnterKey keycode && shiftCtrlAlt == ShiftCtrlAlt False False False then
                when ((listViewListViewFlags .&. EnterAsClickListViewFlag) /= ListViewNoFlags) $
                    join $ lstVwOnClk <$> readMonadIORef rfH
            else case keycode of
                SDL.KeycodeA | not isS && isC && not isA && isMultiSelect -> do
                    (ListViewState{..},_cnt) <- cntUpdt widget
                    liftIO $ VUM.set lstVwMarkers True
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
            (ListViewState{..},cnt,widgSz@(V2 widgW widgH), SDL.Rectangle pC@(P (V2 _xC yC)) _) <- updt widget
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
                                    isCur = i == lstVwCur
                                isMarked <- if not ena || not isMultiSelect
                                            then return False
                                            else liftIO $ VUM.read lstVwMarkers i
                                let ds = DecoreState
                                            ((if | ena && (isMarked || isCur) -> decoreBkColor . selectedDecore
                                                 | ena && ((listViewListViewFlags .&. UseOddColorListViewFlag) /= ListViewNoFlags)
                                                       && odd i -> oddBkColor
                                                 | otherwise -> decoreBkColor . windowDecore) skin)
                                            ((if | not ena -> windowDisabledFgColor
                                                 | isMarked || isCur -> decoreFgColor . selectedDecore
                                                 | otherwise -> decoreFgColor . windowDecore) skin)
--                                liftIO $ putStrLn $ concat ["i=",show i," y=",show y," r=",rectToBriefStr r,
--                                    " r'=",rectToBriefStr r']
                                when ( (listViewListViewFlags .&. DrawItemPrepareListViewFlag) /= ListViewNoFlags) $ do
                                    setColor $ decoreBkColor ds
                                    fillRect r'
                                    when (ena && isCur && isFocused) $ do
                                        setColor $ decoreFgColor ds
                                        drawRect r
                                withClipRect r' $
                                    listViewDrawItem widget skin listViewListViewFlags r ds
                                        ena isFocused isMarked isCur p a i
                                itemDraw $ i + 1
            setColor $ decoreBkColor (windowDecore skin)
            fillRect widgR
            itemDraw $ yC `div` itemH
--            setColor $ rgb 255 0 0
--            drawRect $ shrinkRect' 2 widgR
                                    }

-- | Подготовленные данные для отрисовки в  @listView@  упорядоченного контейнера
--   с элементами поддерживающими 'TextShow'.
newtype ListViewTSPrepare = ListViewTSPrepare Font

-- | Обёртка для использования с @listView@ упорядоченного контейнера с элементами поддерживающими 'TextShow'.
newtype ListViewTS c v = ListViewTS c

-- | Обёртка для использования с @listView@ упорядоченного контейнера с элементами типа 'Text'.
newtype ListViewText c = ListViewText c

-- | Вспомогательная функция для создания @instance ... ListViewable@ с только текстовым отображением элементов.
listViewPrepareTextOnly :: MonadIO m => Canvas m (Coord,Font)
listViewPrepareTextOnly = do
    fnt <- getFont "list"
    fntHeight <- FNT.lineSkip fnt -- FNT.height fnt
    return (fntHeight + 2 * PaddingY,fnt)

-- | Вспомогательная функция для создания @instance ... ListViewable@ с отображением элемента одной строкой.
listViewDrawItemTextOnly :: (MonadIO m, DAROContainer c v, TS.TextShow v) =>
                        Font -> -- ^ Шрифт.
                        GuiRect -> -- ^ Координаты области для рисования.
                        DecoreState -> -- ^ Цвета фона и переднего плана.
                        c -> -- ^ Контейнер с данными.
                        Int -> -- ^ Номер элемента.
                        (v -> T.Text) -> -- ^ Функция преобразования элемента контейнера в 'Text'.
                        Canvas m ()
listViewDrawItemTextOnly fnt rect decoreState c ix f =
    drawTextAligned fnt AlignLeftCenter (decoreFgColor decoreState) (DrawStrOpaque (decoreBkColor decoreState))
        (shrinkRect (V2 PaddingX PaddingY) rect) . f =<< getItemDARO c ix

instance (DAROContainer c v, TS.TextShow v) => ListViewable (ListViewTS c v) ListViewTSPrepare where
    listViewPrepare _skin _listFl _container = fmap ListViewTSPrepare <$> listViewPrepareTextOnly

    -- | Вызывается для отрисовки каждого элемента (строки).
    listViewDrawItem _widget _skin _listFl rect decoreState
                     _isEna _isFocused _isMarked _isCur (ListViewTSPrepare fnt) (ListViewTS c) ix =
        listViewDrawItemTextOnly fnt rect decoreState c ix TS.showt

    -- | Возвращает текущее число элементов.
    listViewGetCount _widget (ListViewTS c) = sizeDARO c

instance DAROContainer c T.Text => ListViewable (ListViewText c) ListViewTSPrepare where
    listViewPrepare _skin _listFl _container = fmap ListViewTSPrepare <$> listViewPrepareTextOnly

    listViewDrawItem _widget _skin _listFl rect decoreState
                     _isEna _isFocused _isMarked _isCur (ListViewTSPrepare fnt) (ListViewText c) ix =
        listViewDrawItemTextOnly fnt rect decoreState c ix id

    listViewGetCount _widget (ListViewText c) = sizeDARO c


-- | Функция создающая окно с @listView@.
popupListView :: (MonadIO m, ListViewable a _p) =>
                -- | Виджет активного сейчас окна. Popup окно станет дочерним по отношению к этому окну.
                Widget ->
                -- | Начальные координаты окна в координатах указанного виджета.
                GuiRect ->
                a -> -- ^ Исходные отображаемые данные.
                Int -> -- ^ Начальный текущий номер ряда (номер элемента).
                (forall n. MonadIO n => Int -> n ()) ->  -- ^ Функция вызываемая при выборе пункта меню
                                                         -- при закрытии окна.
                m ()
popupListView parent rect a ix f = do
    !winSDL <- getSDLWindow =<< getWidgetWindow parent
    win <- mkPopupWindow parent rect
    lstView <- win $+ listView def  { listViewFormItemDef = def{formItemMargin=Just WidgetMarginNone}
                                    , listViewSize = sizeOfRect rect
                                    , listViewListViewFlags = listViewListViewFlags def .|.
                                        EnterAsClickListViewFlag .|. MouseTrackingListViewFlag
                                    , listViewIx = ix
                                    } a
    onClick lstView $ do
        !i <- getIx lstView
--        delWindow win
        SDL.showWindow winSDL >> SDL.raiseWindow winSDL
        f i
    setFocus lstView