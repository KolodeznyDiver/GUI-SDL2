{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:      GUI.Widget.Container.TabbedPanel
-- Copyright:   (c) 2017-2020 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Виджет-контейнер c закладками сверху. Помещаемые в него виджеты располагаются на одном
-- и том же месте, но @WidgetVisible@ остаётся только один, в соответствии с выбраной закладкой.

module GUI.Widget.Container.TabbedPanel(
    -- * Типы используемые с @tabbedPanel@.
    TabItemDef(..),TabbedPanelDef(..),TabbedPanelData
    -- * Функции.
    , tabbedPanel,tabbedPanelIns',tabbedPanelIns,tabbedPanelAppend',tabbedPanelAppend
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Bits
import Data.IORef
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Utils as V
import Data.Default
import Control.Monad.Extra (unlessM,whenJustM,whenJust)
import qualified SDL
import SDL.Vect
import GUI
import GUI.Widget.Handlers
import GUI.Widget.HorizontalItems (pattern OptBtnIx,NeighborSwap(..))
import GUI.Widget.HorizontalTabbeds

pattern AuxWidgCnt :: Int; pattern AuxWidgCnt = 1 -- Кол-во виджетов для внутренних нужд tabbedPanel.

-- | Описание закладки с параметрами по умолчанию.
-- Преобразуется в 'TabItem'.
data TabItemDef = TabItemDef    {
          tabItemCaption :: T.Text -- ^ Текст на закладке.
        , tabItemTextColor :: Maybe GuiColor -- ^ Текст на закладке или @decoreFgColor $ formDecore skin@.
                                }

instance Default TabItemDef where
    def = TabItemDef {
            tabItemCaption = "?"
          , tabItemTextColor = Nothing
                     }

-- | Параметры настройки @tabbedPanel@.
data TabbedPanelDef = TabbedPanelDef {
      tabPanelSize      :: GuiSize -- ^ Размер без полей.
    , tabPanelFlags     :: WidgetFlags -- ^ Флаги базового виджета.
    , tabPanelCanDelete :: Bool -- ^ Можно ли удалять закладку (и для этого добавить справа кнопку.
    , tabPanelPermutable :: Bool -- ^ Допустима перестановка элементов мышью.
                                    }

instance Default TabbedPanelDef where
    def = TabbedPanelDef {
              tabPanelSize = V2 (-1) (-1)
            , tabPanelFlags = WidgetVisible .|. WidgetEnable
            , tabPanelCanDelete = False
            , tabPanelPermutable = False
                        }


-- | Не экспортируемый тип записи. Хранится по ссылке в 'TabbedPanelData'.
data Handlers = Handlers    {
      hndlrOnMove :: forall m. MonadIO m => Int -> m ()
    , hndlrOnRightClick :: forall m. MonadIO m => Int -> m ()
    , hndlrIxUpdate :: forall m. MonadIO m => Int -> m ()
    , hndlrGetIx :: forall m. MonadIO m => m Int
    , hndlrNew :: forall m. MonadIO m => TabItem -> Int -> m ()
    , hndlrSetIndexedValue :: forall m. MonadIO m => Int -> TabItem -> m ()
    , hndlrGetIndexedValue :: forall m. MonadIO m => Int -> m TabItem
                            }

-- Обычно используется как  @GuiWidget TabbedPanelData@.
newtype TabbedPanelData = TabbedPanelData { refHandlers :: IORef Handlers }

-- | Установка и извлечение номера текущей (видимой) закладки.
instance IxProperty (GuiWidget TabbedPanelData) where
    setIx w ix = do h <- readMonadIORef $ refHandlers $ widgetData w
                    hndlrIxUpdate h ix
    getIx w = do h <- readMonadIORef $ refHandlers $ widgetData w
                 hndlrGetIx h

-- | Установка функции-обработчика изменение номера текущей (видимой) закладки.
instance Moveable (GuiWidget TabbedPanelData) Int where
    onMove w a = modifyMonadIORef' (refHandlers $ widgetData w) (\d -> d{hndlrOnMove= a})

-- | Установка функции-обработчика щелчка ПКМ.
-- Значение аргумента @OptBtnIx@ означает что дейтвие выполнено для опциональной кнопки,
-- иначе для указанной закладки.
instance RightClickable1 (GuiWidget TabbedPanelData) Int where
    onRightClick1 w a = modifyMonadIORef' (refHandlers $ widgetData w) (\d -> d{hndlrOnRightClick= a})

-- | Установка и извлечение 'TabItem' закладки по номеру.
instance IndexedValueProperty (GuiWidget TabbedPanelData) TabItem where
    setIndexedValue w ix v = do h <- readMonadIORef $ refHandlers $ widgetData w
                                hndlrSetIndexedValue h ix v
    getIndexedValue w ix  = do h <- readMonadIORef $ refHandlers $ widgetData w
                               hndlrGetIndexedValue h ix

-- | Функция создания виджета-контейнера прокручивамой области.
tabbedPanel :: MonadIO m => TabbedPanelDef ->  -- ^ Параметры виджета.
                           Widget -> -- ^ Будущий предок в дереве виджетов.
                           Skin -> -- ^ Skin.
                           m (GuiWidget TabbedPanelData)
tabbedPanel TabbedPanelDef{..} parent skin = do
    rfTabH <- newMonadIORef 0
    rfH <- newMonadIORef Handlers{ hndlrOnMove = \_ -> return ()
                                 , hndlrOnRightClick = \_ -> return ()
                                 , hndlrIxUpdate = \_ -> return ()
                                 , hndlrGetIx = return 0
                                 , hndlrNew = \_ _ -> return ()
                                 , hndlrSetIndexedValue = \_ _ -> return ()
                                 , hndlrGetIndexedValue = undefined
                                 }

    let initScrllArFns = noChildrenFns tabPanelSize
        arrangeChild :: MonadIO m => GuiRect -> Coord -> Int -> Widget -> m ()
        arrangeChild (SDL.Rectangle selfLT@(P (V2 selfX selfY)) (V2 selfW selfH))
                            h ix child =
            if ix == 0 then
                when (h >0) $ do
                    writeMonadIORef rfTabH h
                    widgetResizingIfChanged child $ SDL.Rectangle selfLT (V2 selfW h)
            else do
                tabH <- readMonadIORef rfTabH
                when (tabH>0) $
                    widgetResizingIfChanged child $
                        SDL.Rectangle (P (V2 selfX (selfY+tabH))) (V2 selfW (selfH-tabH))



    self <- mkWidget tabPanelFlags WidgetMarginNone
                        (TabbedPanelData rfH) parent initScrllArFns{
                onSizeChangedParentNotify = \widget child (V2 _w h) -> do
                    mbI <- getChildWidgetIx widget child
                    whenJust mbI $ \ix -> do
                        selfRect <- getWidgetRect widget
                        arrangeChild selfRect h ix child
              , onResizing = \widget newRect -> do
                    void $ simpleOnResizing widget newRect
                    let byChild ix child = do
                            tabH <- readMonadIORef rfTabH
                            arrangeChild newRect tabH ix child
                    imapByWidgetChildren_  byChild widget
                                                            }
    let selfWidget = baseWidget self

    ht  <- horizTab def{hTabFormItemDef=def{
                  formItemMargin= Just WidgetMarginNone}
                , hTabCanDelete = tabPanelCanDelete
                , hTabPermutable = tabPanelPermutable
                                           } selfWidget skin


    let whenNotEmpty f = do
            maxItem <- pred <$> getWidgetChildrenCount selfWidget
            when (maxItem >= AuxWidgCnt) $ f maxItem


        doDel :: MonadIO m => Int -> m ()
        doDel pos = whenNotEmpty $ \maxItem -> do
                        setValue ht . V.unsafeDelElemByIx pos =<< getValue ht
                        iCur <- getIx ht
                        let maxItem' = maxItem - AuxWidgCnt
                        if iCur > maxItem' then setIx ht maxItem'
                        else markWidgetForRedraw selfWidget

        -- Новый виджет уже вставлен в дерево виджетов, но в конец, а вот TabItem ещё не вставлен.
        doNew :: MonadIO m => TabItem -> Int -> m ()
        doNew ti pos = whenNotEmpty $ \maxItem -> do
                let pos' = max pos 0
                    maxPanelIx = maxItem - AuxWidgCnt
                whenJustM (getWidgetChild selfWidget maxItem) $ \newWidg -> do
                    fns <- getWidgetFns newWidg
                    setWidgetFns newWidg fns{
                        onDestroy = \widget -> do
                            whenJustM (getChildWidgetIx selfWidget widget) $ \ix ->
                                when (ix >= AuxWidgCnt) $
                                    doDel (ix-AuxWidgCnt)
                            onDestroy fns widget
                                            }
                v <- getValue ht
                v' <- if pos' <= maxPanelIx then do
                        swapChildWidgets selfWidget pos' maxPanelIx
                        return $ V.insert pos' ti v
                      else return $ V.snoc v ti
                setValue ht v'
                setIx ht $ min pos' maxPanelIx

        visibleStateUpdate iCur = imapByWidgetChildren_ f selfWidget
            where iCur' = iCur + AuxWidgCnt
                  f 0 _ = return ()
                  f ix child | ix == iCur' =
                                unlessM (allWidgetFlags child WidgetVisible) $ do
                                    widgetFlagsAdd child WidgetVisible
                                    markWidgetForRedraw child
                             | otherwise = widgetFlagsRemove child WidgetVisible

    modifyMonadIORef' rfH $ \h -> h{
          hndlrNew= doNew
        , hndlrIxUpdate = setIx ht
        , hndlrGetIx = getIx ht
        , hndlrSetIndexedValue = \ix newV ->
            setValue ht . (V.// [(ix,newV)]) =<< getValue ht
        , hndlrGetIndexedValue = \ix -> ( V.! ix) <$> getValue ht
                                    }

    setNeighborSwap ht $ \ix v -> do
        swapChildWidgets selfWidget ix (ix+1)
        return $ V.swapNeighb ix v

    onMove ht $ \ix -> do
        visibleStateUpdate ix
        h <- readMonadIORef rfH
        hndlrOnMove h ix

    onClick1 ht $ \ix ->
        when ( ix == OptBtnIx ) $ do
            iCur <- getIx ht
            whenJustM (getWidgetChild selfWidget iCur)
                delWidget

    onRightClick1 ht $ \ix -> do
        h <- readMonadIORef rfH
        hndlrOnRightClick h ix

    return self

-- | Реализация вставки виджета в tabbedPanel.
tabbedPanelIns' :: MonadIO m =>
                  GuiWidget TabbedPanelData -> -- ^ Контейнер в который добавляется элемент (и закладка).
                  Int ->  -- ^ Новая позиция добавляемого элемента. Если >= кол-ву элементов, то
                          -- вставляется в конец.
                  TabItem -> -- ^ Описание добавляемой закладки
                  (Widget -> Skin -> m (GuiWidget b)) -> -- ^ Функция нового виджета.
                  m (GuiWidget b)
tabbedPanelIns' w pos tabItem initF = do
    newGW <- createWidget (baseWidget w) initF
    h <- readMonadIORef (refHandlers $ widgetData w)
    hndlrNew h tabItem pos
    return newGW


tabbedPanelIns :: MonadIO m =>
                  GuiWidget TabbedPanelData -> -- ^ Контейнер в который добавляется элемент (и закладка).
                  Int ->  -- ^ Новая позиция добавляемого элемента. Если >= кол-ву элементов, то
                          -- вставляется в конец.
                  TabItemDef -> -- ^ Описание добавляемой закладки
                  (Widget -> Skin -> m (GuiWidget b)) -> -- ^ Функция нового виджета.
                  m (GuiWidget b)
tabbedPanelIns w pos TabItemDef{..} initF = do
    color <- case tabItemTextColor of
                Just c -> return c
                _ -> (decoreFgColor . formDecore) <$> getSkinFromWidget (baseWidget w)
    tabbedPanelIns' w pos (TabItem tabItemCaption color) initF


tabbedPanelAppend' :: MonadIO m =>
                     GuiWidget TabbedPanelData -> -- ^ Контейнер в который добавляется элемент (и закладка).
                     TabItem -> -- ^ Описание добавляемой закладки
                     (Widget -> Skin -> m (GuiWidget b)) -> -- ^ Функция нового виджета.
                     m (GuiWidget b)
tabbedPanelAppend' w = tabbedPanelIns' w 99999

tabbedPanelAppend :: MonadIO m =>
                     GuiWidget TabbedPanelData -> -- ^ Контейнер в который добавляется элемент (и закладка).
                     TabItemDef -> -- ^ Описание добавляемой закладки
                     (Widget -> Skin -> m (GuiWidget b)) -> -- ^ Функция нового виджета.
                     m (GuiWidget b)
tabbedPanelAppend w = tabbedPanelIns w 99999
