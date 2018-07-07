{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
-- |
-- Module:      GUI.Widget.HorizontalLinks
-- Copyright:   (c) 2017-2018 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Поле с текстовыми ссылками (частями строк по которым можно щёлкать) разделёнными текстовым
-- разделителем, с возможностью горизонтальной прокрутки.
-- Использует шрифт \"link\" из таблицы шрифтов менеджера ресурсов для отображения самих ссылок и
-- \"separator\" для отображения разделителей.

module GUI.Widget.HorizontalLinks(
    -- * Типы используемые с @horizLinks@.
    LinkVector,HorizLinksDef(..),HorizLinksData
    -- * Функция создания виджета с текстовыми ссылками разделёнными текстовым разделителем.
    ,horizLinks
    ) where

import Data.Bits
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Default
import SDL.Vect
import qualified SDL.Font as FNT
import SDL.Font (Font)
import GUI
import Data.Container.DirectAccess
import GUI.Utils.ViewableItems
import GUI.Widget.HorizontalItems

pattern PaddingX :: Coord
pattern PaddingX = 5
pattern PaddingY :: Coord
pattern PaddingY = 3

type LinkVector = V.Vector T.Text

-- | Начальные настройки виджета.
data HorizLinksDef = HorizLinksDef {
          horizLnFormItemDef  :: FormItemWidgetDef -- ^ Общие настройки для всех виджетов для форм,
                                                    -- в настоящий момент только margin's.
        , horizLnWidth   :: Coord -- ^ Ширина поля редактирования. Высота определяется размером шрифта
                                   -- и внутренними полями фиксированного размера.
        , horizLnFlags   :: WidgetFlags -- ^ Флаги базового виджета.
        , horizLnFirstSep    :: T.Text -- ^ Начальный разделитель.
        , horizLnSep    :: T.Text -- ^ Разделитель между элементами.
        , horizLnLastSep   :: T.Text -- ^ Конечный разделитель.
--        , horizLnLinks :: LinkVector -- ^ Начальные ссылки.
        , horizLnOptBtn  :: T.Text -- ^ Имя файла из ресурсов для дополнительной (самой правой) кнопки.
                                   --   Если пустая строка, то дополнительной кнопки нет.
        , horizLnMoveOnUpdate :: MoveOnUpdate -- ^  Как перемещать видимое окно списка при обновлении данных.
                               }

instance Default HorizLinksDef where
    def = HorizLinksDef { horizLnFormItemDef = def
                        , horizLnWidth = -1
                        , horizLnFlags = WidgetVisible .|. WidgetEnable
                        , horizLnFirstSep = T.empty
                        , horizLnSep = " | "
                        , horizLnLastSep = T.empty
--                        , horizLnLinks = V.empty
                        , horizLnOptBtn = T.empty
                        , horizLnMoveOnUpdate = CurVisibleOnUpdate
                        }


-- | Подготовленные данные для отрисовки в  @horizLinks@  тестовых ссылок.
newtype HorizLinksPrepare = HorizLinksPrepare Font

-- | Подготовленные данные для отрисовки в  @horizLinks@  тестовых сепараторов.
-- newtype HorizLinksSepPrepare = HorizLinksSepPrepare Font

-- | Обёртка для использования с @horizLinks@ 'LinkVector'.
newtype HorizLinks = HorizLinks { unHorizLinks :: LinkVector}
                    deriving Eq

-- | Параметры текстовых сепараторов
data HorizLinksSeparators = HorizLinksSeparators {
          firstSep :: T.Text
        , middleSep :: T.Text
        , lastSep :: T.Text
                                                 }

instance ViewableItems HorizLinks HorizLinksPrepare where
    viewablePrepare _gui _skin _container = fmap HorizLinksPrepare <$> viewablePrepareTextOnly "link"

    -- | Вызывается для отрисовки каждого элемента (строки).
    viewableDrawItem _gui skin rect decoreState
                     isEna _isFocused inMouse _isCur (HorizLinksPrepare fnt) c ix =
        viewableDrawItemTextOnly fnt rect
            decoreState{decoreFgColor = if | not isEna -> formDisabledFgColor skin
                                           | inMouse -> linkInFgColor skin
                                           | otherwise -> linkFgColor skin
                       }
            (unHorizLinks c) ix id

    -- | Возвращает текущее число элементов.
    viewableGetCount = sizeDARO . unHorizLinks

instance ViewableVarWidthItems HorizLinks HorizLinksPrepare where
    viewableGetWidth _gui (HorizLinksPrepare fnt) c ix = xV2 <$> getTextSize fnt (unHorizLinks c V.! ix)

instance HorizontalSeparator HorizLinksSeparators Font where
    horizSepPrepare _gui _skin  HorizLinksSeparators{..} = do
        fnt <- getFont "separator"
        fntHeight <- FNT.lineSkip fnt -- FNT.height fnt
        firstW <- xV2 <$> getTextSize fnt firstSep
        middleW <- xV2 <$> getTextSize fnt middleSep
        lastW <- xV2 <$> getTextSize fnt lastSep
        return HorizSepPrepare{
                         horizSepPrFirstWidth = firstW
                       , horizSepPrMiddleWidth = middleW
                       , horizSepPrLastWidth = lastW
                       , horizSepPrHeight = fntHeight + 2 * PaddingY
                       , horizSepPrData = fnt
                              }

    -- | Вызывается для отрисовки каждого разделителя.
    horizSepDrawItem _gui skin rect fnt HorizLinksSeparators{..} sepPos _sepCurPos =
        drawTextAligned fnt AlignLeftCenter (decoreFgColor $ formDecore skin)
            (DrawStrOpaque (decoreBkColor $ formDecore skin)) (shrinkRect (V2 PaddingX PaddingY) rect)
            (case sepPos of
                FirstSeparator -> firstSep
                MiddleSeparator -> middleSep
                LastSeparator -> lastSep
            )

-- | Тип созданного виджета. Обычно используется как  @GuiWidget HorizLinksData@.
newtype HorizLinksData = HorizLinksData { dat :: HorizItsData HorizLinks HorizLinksPrepare }

-- | Установка и извлечение номера текущего (выделенного) элемента.
instance IxProperty (GuiWidget HorizLinksData) where
    setIx w = setIx w{widgetData= dat $ widgetData w}
    getIx w = getIx w{widgetData= dat $ widgetData w}

-- | Установка функции-обработчика изменение текущей позиции (номера выбранного элемента).
-- Вызывается перед __/(Dbl)Clk/__ если номер меняется, а так же при изменении номера текущего элемента
-- через setIx.
instance Moveable (GuiWidget HorizLinksData) Int where
    onMove w = onMove w{widgetData= dat $ widgetData w}

-- | Установка функции-обработчика одинарного щелчка.
-- Значение аргумента @OptBtnIx@ означает что дейтвие выполнено для опциональной кнопки,
-- иначе для указанного номера элемента.
instance Clickable1 (GuiWidget HorizLinksData) Int where
    onClick1 w = onClick1 w{widgetData= dat $ widgetData w}

-- | Установка функции-обработчика двойного щелчка.
-- Значение аргумента @OptBtnIx@ означает что дейтвие выполнено для опциональной кнопки,
-- иначе для указанного номера элемента.
instance DoubleClickable1 (GuiWidget HorizLinksData) Int where
    onDoubleClick1 w = onDoubleClick1 w{widgetData= dat $ widgetData w}

-- | Установка функции-обработчика одинарного щелчка правой кнопкой указателя.
-- Значение аргумента @OptBtnIx@ означает что дейтвие выполнено для опциональной кнопки,
-- иначе для указанного номера элемента.
instance RightClickable1 (GuiWidget HorizLinksData) Int where
    onRightClick1 w = onRightClick1 w{widgetData= dat $ widgetData w}

-- | Установка и извлечение набора ссылок.
instance ValueProperty (GuiWidget HorizLinksData) LinkVector where
    setValue w v = setValue w{widgetData= dat $ widgetData w} $ HorizLinks v
    getValue w = unHorizLinks <$> getValue w{widgetData= dat $ widgetData w}

-- | Функция создания виджета с текстовыми ссылками разделёнными текстовым разделителем.
horizLinks :: MonadIO m => HorizLinksDef -> -- ^ Параметры виджета.
                         Widget -> -- ^ Будущий предок в дереве виджетов.
                         Skin -> -- ^ Skin.
                         m (GuiWidget HorizLinksData)
horizLinks HorizLinksDef{..} parent skin = do
    widg <- horizItems def{ horizItsFormItemDef = horizLnFormItemDef
                          , horizItsWidth = horizLnWidth
                          , horizItsFlags = horizLnFlags
                          , horizItsOptBtn = horizLnOptBtn
                          , horizItsMoveOnUpdate = horizLnMoveOnUpdate
                          }
                HorizLinksSeparators {
                          firstSep = horizLnFirstSep
                        , middleSep = horizLnSep
                        , lastSep = horizLnLastSep
                                     }
                (HorizLinks V.empty) parent skin
    return widg{widgetData= HorizLinksData $ widgetData widg}
