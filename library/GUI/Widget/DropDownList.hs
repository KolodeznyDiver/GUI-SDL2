{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:      GUI.Widget.DropDownList
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <kolodeznydiver@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Виджет отображения нередактируемого поля с возможностью вызова выпадающего списка для выбора варианта.

module GUI.Widget.DropDownList(
    -- * Типы используемые в dropDownList
    DropDownListDef(..),DropDownListData
    -- * Функция создания виджета.
    ,dropDownList
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Bits
import Data.IORef
import qualified SDL
import SDL.Vect
import Control.Monad.Extra (whenM,whenJustM)
import Data.Default
import GUI
import qualified GUI.BaseLayer.Primitives as P
import GUI.Widget.Handlers
import GUI.Utils.ViewableItems
import GUI.Widget.Button
import GUI.Widget.ListView

pattern PaddingX :: Coord
pattern PaddingX = 5
pattern PaddingY :: Coord
pattern PaddingY = 3

-- | Начальные настройки виджета.
data DropDownListDef = DropDownListDef {
          ddListFormItemDef :: FormItemWidgetDef -- ^ Общие настройки для всех виджетов для форм,
                                                   -- в настоящий момент только margin's.
        , ddListSize       :: GuiSize -- ^ Размер без полей.
                                      -- высота будет установлена не менее требуемой для отображения
                                      -- элементов т.е. возвращаемая @viewablePrepare@ @+PaddingY@.
                                      -- Можно указывать высоту 0 для автоопределения высоты виджета.
        , ddListFlags      :: WidgetFlags -- ^ Флаги базового виджета.
        , ddListListViewFlags  :: Maybe ListViewFlags -- ^ Флаги виджета выпадающего списка,
                                                      -- если желательно изменить.
                                                      -- Флаги EnterAsClickListViewFlag и MouseTrackingListViewFlag
                                                      -- в любом случае будут установлены, а
                                                      -- MultiSelectListViewFlag сброшен.
        , ddListPopupHeight :: Coord -- ^ Высота выпадающего списка, пикселей.
        , ddListIx     :: Int -- ^ Начальный текущий номер ряда (номер элемента).
                               }


instance Default DropDownListDef where
    def = DropDownListDef { ddListFormItemDef = def
                          , ddListSize = zero
                          , ddListFlags = WidgetVisible .|. WidgetEnable .|. WidgetFocusable .|. WidgetTabbed
                          , ddListListViewFlags = Nothing
                          , ddListPopupHeight = 100
                          , ddListIx = 0
                          }


-- | Тип созданного виджета. Обычно используется как  @GuiWidget DropDownListData@.
data DropDownListData a p = DropDownListData    { ddLstDat :: a
                                                , ddLstOnChanged :: IORef (OneArgAction Int)
                                                , ddLstIx :: IORef Int
                                                }

-- | Установка функции-обработчика на изменение номера выбранного элемента.
instance Changeable (GuiWidget (DropDownListData a p)) Int where
    onChanged w a = writeMonadIORef (ddLstOnChanged $ widgetData w) $ OneArgAction a

-- | Установка и извлечение номера выбранного элемента.
instance ViewableItems a p => IxProperty (GuiWidget (DropDownListData a p)) where
    setIx w v = do
        let widget = baseWidget w
            DropDownListData{..} = widgetData w
        old <- readMonadIORef ddLstIx
        when ( (v /= old) && (v>=0)) $ do
            count <- viewableGetCount ddLstDat
            when (v < count) $ do
                writeMonadIORef ddLstIx v
                readMonadIORef ddLstOnChanged >>= ( ( $ v) . oneArgAction)
                markWidgetForRedraw widget
    getIx = readMonadIORef . ddLstIx . widgetData

-- | Функция создания виджета списка.
dropDownList :: (MonadIO m, ViewableItems a p) =>
                         DropDownListDef -> -- ^ Параметры виджета.
                         a -> -- ^ Отображаемые в виде списка данные. Выбранный элемент отображается в
                              -- основном поле виджета.
                         Widget -> -- ^ Будующий предок в дереве виджетов.
                         Skin -> -- ^ Skin.
                         m (GuiWidget (DropDownListData a p))
dropDownList DropDownListDef{..} a parent skin = do
    let listViewFlags = complement MultiSelectListViewFlag .&. (
            EnterAsClickListViewFlag .|. MouseTrackingListViewFlag .|.
            fromMaybe (listViewListViewFlags def) ddListListViewFlags)
    gui <- getGuiFromWidget parent
    (itemH,p) <- runProxyCanvas parent $ viewablePrepare gui skin a
    rfOnChanged <- newMonadIORef $ OneArgAction (\_ -> return ())
    rfIx <- newMonadIORef ddListIx
    btTexture <- runProxyCanvas parent $ getTexture "ScrollAreaArrBtns.png"
    btWH <- ((`div` 2).xV2) <$> P.getTextureSize btTexture
    let  widgSz@(V2 widgW widgH) = let (V2 w h) = ddListSize in
                                   V2 w (max (itemH+PaddingY*2) h)
         fns = noChildrenFns widgSz
         doChange widget newIx = do
            old <- readMonadIORef rfIx
            when (newIx /= old)
                (writeMonadIORef rfIx newIx >> markWidgetForRedraw widget >> readMonadIORef rfOnChanged >>=
                 ( $ newIx) . oneArgAction)
         doPopupList widget = do
            (SDL.Rectangle _ (V2 w h)) <- getWidgetRect widget
            ix <- readMonadIORef rfIx
            count <- viewableGetCount a
            popupListView widget (SDL.Rectangle (P (V2 0 h)) (V2 w (min ddListPopupHeight (itemH*count))))
                (Just listViewFlags) a ix
                (\ i -> doChange widget i >> setWidgetFocus widget)
         arrangeChild widget =
            whenJustM (getWidgetChild widget 0) $ \child ->
                -- Можно вызывать widgetResizingIfChanged, т.к. у кнопки WidgetMarginNone
                widgetResizingIfChanged child $
                    SDL.Rectangle (P (V2 (widgW-btWH-2*PaddingX) ((widgH - btWH) `div` 2))) (V2 btWH btWH)

    self <- mkWidget ddListFlags (fromMaybe (formItemsMargin skin) $ formItemMargin ddListFormItemDef)
                        (DropDownListData a rfOnChanged rfIx) parent fns{
        onSizeChangedParentNotify = \widget _child _sz -> arrangeChild widget
        , onResizing = \widget newRect -> do
            void $ simpleOnResizing widget newRect
            arrangeChild widget
        ,onMouseButton = \widget motion mouseButton _clicks _pnt ->
            when ((motion==SDL.Pressed) && (mouseButton == SDL.ButtonLeft)) $ doPopupList widget
        ,onKeyboard = \widget motion _repeated keycode _km -> when (motion==SDL.Pressed) $
            when (isEnterKey keycode || keycode == SDL.KeycodeDown) $ doPopupList widget
        ,onDraw= \widget -> do
            fl <- getWidgetFlags widget
            let widgR = SDL.Rectangle zero widgSz
                ena = (fl .&. WidgetEnable) /= WidgetNoFlags
                isFocused = (fl .&. WidgetFocused) /= WidgetNoFlags
                ds = if | not ena -> DecoreState (decoreBkColor $ windowDecore skin) (windowDisabledFgColor skin)
                        | isFocused -> selectedDecore skin
                        | otherwise -> windowDecore skin
            drawRoundFrame (decoreBkColor (formDecore skin)) (formBorderColor skin) (decoreBkColor ds) widgR
            ix <- readMonadIORef rfIx
            viewableDrawItem gui skin
                (SDL.Rectangle (P (V2 PaddingX PaddingY)) (V2 (widgW-btWH-3*PaddingX) itemH))
                ds ena isFocused False False p a ix
                                                                       }
    let selfWidget = baseWidget self
    btDn <- textureButton def{ buttonFormItemDef = def{formItemMargin= Just WidgetMarginNone}
                             , buttonSize = V2 btWH btWH
                             , buttonFlags = WidgetVisible .|. WidgetEnable
                             , buttonTexture = btTexture
                             , buttonMouseInPictIx = Just 1
                             , buttonPressedPictIx = Just 1
                             , buttonInitPictRow = 3
                             } selfWidget skin
    fnsCorrectionForTransparent btDn
    onClick btDn $
        whenM (allWidgetFlags selfWidget WidgetEnable) $
           doPopupList selfWidget
    return self
