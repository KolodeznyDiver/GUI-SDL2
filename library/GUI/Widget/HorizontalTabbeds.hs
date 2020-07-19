{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
-- |
-- Module:      GUI.Widget.HorizontalTabbeds
-- Copyright:   (c) 2017-2020 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Поле с закладками. Верхний элемент панели с закладками @tabbedPanel@.

module GUI.Widget.HorizontalTabbeds(
    -- * Типы используемые с @horizTab@.
    TabItem(..),TabVector,HorizTabDef(..),HorizTabData
    -- * Функции.
    ,horizTab
    ) where

import Data.Bits
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Default
import qualified SDL
import SDL.Vect
import qualified SDL.Font as FNT
import SDL.Font (Font)
import GUI
import Data.Container.DirectAccess
import GUI.Utils.ViewableItems
import GUI.Widget.HorizontalItems

pattern PaddingX :: Coord; pattern PaddingX = 5
pattern PaddingTop :: Coord; pattern PaddingTop = 6
pattern PaddingTopCur :: Coord; pattern PaddingTopCur = 3
pattern BorderThickness :: Coord; pattern BorderThickness = 1
pattern PaddingBottom :: Coord; pattern PaddingBottom = 3
pattern SeparatorWidth :: Coord; pattern SeparatorWidth = 1

data TabItem = TabItem  { tabCaption :: T.Text
                        , tabTextColor :: GuiColor
                        }
                 deriving Eq

type TabVector = V.Vector TabItem

-- | Начальные настройки виджета.
data HorizTabDef = HorizTabDef {
          hTabFormItemDef  :: FormItemWidgetDef -- ^ Общие настройки для всех виджетов для форм,
                                                    -- в настоящий момент только margin's.
        , hTabWidth   :: Coord -- ^ Ширина поля редактирования. Высота определяется размером шрифта
                                   -- и внутренними полями фиксированного размера.
        , hTabFlags   :: WidgetFlags -- ^ Флаги базового виджета.
        , hTabMoveOnUpdate :: MoveOnUpdate -- ^  Как перемещать видимое окно списка при обновлении данных.
        , hTabCanDelete :: Bool -- ^ Можно ли удалять закладку (и для этого добавить справа кнопку.
        , hTabPermutable :: Bool -- ^ Допустима перестановка элементов мышью.
                               }

instance Default HorizTabDef where
    def = HorizTabDef { hTabFormItemDef = def
                        , hTabWidth = -1
                        , hTabFlags = WidgetVisible .|. WidgetEnable
                        , hTabMoveOnUpdate = CurVisibleOnUpdate
                        , hTabCanDelete = False
                        , hTabPermutable = False
                        }


-- | Подготовленные данные для отрисовки закладок в @horizTab@.
newtype HorizTabPrepare = HorizTabPrepare Font

-- | Тип описания разделителя.
data HorizTabSeparators = HorizTabSeparators

getTabTop :: Bool -> Coord
getTabTop b = if b then PaddingTopCur else PaddingTop
{-# INLINE getTabTop #-}

instance ViewableItems TabVector HorizTabPrepare where
    viewablePrepare _gui _skin _container = do
        fnt <- getFont "label"
        fntHeight <- FNT.lineSkip fnt -- FNT.height fnt
        return (fntHeight + PaddingTop + PaddingBottom + BorderThickness, HorizTabPrepare fnt)

    -- | Вызывается для отрисовки каждого элемента (строки).
    viewableDrawItem _gui skin (SDL.Rectangle (P (V2 x y)) (V2 w h)) _decoreState
                     isEna _isFocused inMouse isCur (HorizTabPrepare fnt) c ix = do
        let dY = getTabTop isCur
            top = y + dY
            TabItem{..} = c V.! ix
            bgClr = decoreBkColor $ (if | not isEna -> btnDecoreDisabled . formItemsButtons
                                        | inMouse -> btnDecoreIn . formItemsButtons
                                        | isCur -> windowDecore
                                        | otherwise -> btnDecoreOut . formItemsButtons
                                    ) skin
        setColor bgClr
        fillRect $ SDL.Rectangle (P (V2 (x+1) (top+1))) (V2 (w-2) (h-dY-1))
        setColor $ formBorderColor skin
        drawLine (P (V2 (x+1) top)) (P (V2 (x+w-2) top))
        drawPoint (P (V2 x (top+1)))
        drawPoint (P (V2 (x+w-1) (top+1)))
        drawTextOpaque fnt ( if isEna then tabTextColor
                             else decoreFgColor $ btnDecoreDisabled $ formItemsButtons skin)
                bgClr (P (V2 (x+PaddingX) (y+PaddingTop+BorderThickness))) tabCaption

    -- | Возвращает текущее число элементов.
    viewableGetCount = sizeDARO

instance ViewableVarWidthItems TabVector HorizTabPrepare where
    viewableGetWidth _gui (HorizTabPrepare fnt) c ix =
        ((+ PaddingX*2) . xV2) <$> getTextSize fnt (tabCaption $ c V.! ix)

instance HorizontalSeparator HorizTabSeparators () where
    horizSepPrepare _gui _skin  _ =
        return HorizSepPrepare{
                         horizSepPrFirstWidth = SeparatorWidth
                       , horizSepPrMiddleWidth = SeparatorWidth
                       , horizSepPrLastWidth = SeparatorWidth
                       , horizSepPrHeight = 0
                       , horizSepPrData = ()
                              }

    -- | Вызывается для отрисовки каждого разделителя.
    horizSepDrawItem _gui skin (SDL.Rectangle (P (V2 x y)) (V2 _w h)) _ _ _sepPos sepCurPos = do
        setColor $ formBorderColor skin
        drawLine (P (V2 x (y + 2 + getTabTop (sepCurPos /= OrdinalSeparator))))
                 (P (V2 x (y+h)))

-- | Тип созданного виджета. Обычно используется как  @GuiWidget HorizTabData@.
newtype HorizTabData = HorizTabData { dat :: HorizItsData TabVector HorizTabPrepare }

-- | Установка и извлечение номера текущего (выделенного) элемента.
instance IxProperty (GuiWidget HorizTabData) where
    setIx w = setIx w{widgetData= dat $ widgetData w}
    getIx w = getIx w{widgetData= dat $ widgetData w}

instance NeighborSwap (GuiWidget HorizTabData) TabVector where
    setNeighborSwap w = setNeighborSwap w{widgetData= dat $ widgetData w}

-- | Установка функции-обработчика изменение текущей позиции (номера выбранного элемента).
-- Вызывается перед __/(Dbl)Clk/__ если номер меняется, а так же при изменении номера текущего элемента
-- через setIx.
instance Moveable (GuiWidget HorizTabData) Int where
    onMove w = onMove w{widgetData= dat $ widgetData w}

-- | Установка функции-обработчика одинарного щелчка.
-- Значение аргумента @OptBtnIx@ означает что дейтвие выполнено для опциональной кнопки,
-- иначе для указанного номера элемента.
instance Clickable1 (GuiWidget HorizTabData) Int where
    onClick1 w = onClick1 w{widgetData= dat $ widgetData w}

-- | Установка функции-обработчика двойного щелчка.
-- Значение аргумента @OptBtnIx@ означает что дейтвие выполнено для опциональной кнопки,
-- иначе для указанного номера элемента.
instance DoubleClickable1 (GuiWidget HorizTabData) Int where
    onDoubleClick1 w = onDoubleClick1 w{widgetData= dat $ widgetData w}

-- | Установка функции-обработчика одинарного щелчка правой кнопкой указателя.
-- Значение аргумента @OptBtnIx@ означает что дейтвие выполнено для опциональной кнопки,
-- иначе для указанного номера элемента.
instance RightClickable1 (GuiWidget HorizTabData) Int where
    onRightClick1 w = onRightClick1 w{widgetData= dat $ widgetData w}

-- | Установка и извлечение набора ссылок.
instance ValueProperty (GuiWidget HorizTabData) TabVector where
    setValue w = setValue w{widgetData= dat $ widgetData w}
    getValue w = getValue w{widgetData= dat $ widgetData w}

-- | Функция создания виджета с текстовыми ссылками разделёнными текстовым разделителем.
horizTab :: MonadIO m => HorizTabDef -> -- ^ Параметры виджета.
                         Widget -> -- ^ Будущий предок в дереве виджетов.
                         Skin -> -- ^ Skin.
                         m (GuiWidget HorizTabData)
horizTab HorizTabDef{..} parent skin = do
    widg <- horizItems def{ horizItsFormItemDef = hTabFormItemDef
                          , horizItsWidth = hTabWidth
                          , horizItsFlags = hTabFlags
                          , horizItsOptBtn = if hTabCanDelete then "close.png"
                                             else T.empty
                          , horizItsMoveOnUpdate = hTabMoveOnUpdate
                          , horizItsPermutable = hTabPermutable
                          }
                HorizTabSeparators V.empty parent skin
    return widg{widgetData= HorizTabData $ widgetData widg}
