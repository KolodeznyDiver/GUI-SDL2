{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
-- |
-- Module:      GUI.Utils.ViewableItems
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <kolodeznydiver@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Некие контейнеры элементы которых можно отображать графически.
--
-- В данных классах не конкретезируется способ хранения или извлечения данных,
-- а только отрисовка.

module GUI.Utils.ViewableItems(
    -- * Классы задающие отображение элементов.
    ViewableItems(..),ViewableVarWidthItems(..)
    -- * Горизонтальный разделитель элементов.
    ,HorizSepPos(..),HorizSepCurPos(..),HorizSepPrepare(..),HorizontalSeparator(..)
    -- * Вспомогательные функции.
    ,viewablePrepareTextOnly,viewableDrawItemTextOnly
    ) where

import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified TextShow as TS
import qualified SDL.Font as FNT
import SDL.Vect
import SDL.Font (Font)
import GUI
import Data.Container.DirectAccess

pattern PaddingX :: Coord
pattern PaddingX = 5
pattern PaddingY :: Coord
pattern PaddingY = 3

-- | Контейнер с произвольной отрисовкой элементов в, предположительно прямоугольных областях.
class ViewableItems a p | a -> p where
    -- | Вызывается перед использованием, при создании виджета, через @runProxyCanvas@.
    viewablePrepare :: MonadIO m =>
                       Gui ->
                       Skin ->
                       a -> -- ^ Отображаемые данные (могут изменться во время существования Widget-а.
                       Canvas m (Coord,p) -- ^ Высота строки и подготовленные данные для отрисовки.

    -- | Вызывается для отрисовки каждого элемента.
    viewableDrawItem :: MonadIO m =>
                        Gui ->
                        Skin ->
                        GuiRect -> -- ^ Координаты области для рисования.
                        DecoreState -> -- ^ Цвета фона и переднего плана.
                        Bool -> -- ^ WidgetEnable ?
                        Bool -> -- ^ WidgetFocused ?
                        Bool -> -- ^ Элемент маркированный.
                        Bool -> -- ^ Текущий ли это элемент.
                        p -> -- ^ Данные для отрисовки сформированные в @viewablePrepare@.
                        a -> -- ^ Отображаемые данные.
                        Int -> -- ^ Номер элемента.
                        Canvas m ()

    -- | Может ли указанный элемент быть отмеченным (выбранным, selected).
    viewableCanSelect :: MonadIO m =>
                        Gui ->
                        a -> -- ^ Отображаемые данные.
                        Int -> -- ^ Номер элемента.
                        m Bool
    viewableCanSelect _gui _a _ix = return True

    -- | Возвращает текущее число элементов.
    viewableGetCount :: MonadIO m =>
                        a -> -- ^ Отображаемые данные.
                        m Int

    -- | Вызывается после завершения использования для освобождения ресурсов выделенных во @viewablePrepare@.
    viewableUnprepare :: MonadIO m =>
                        Gui ->
                        p -> -- ^ Данные для отрисовки сформированные в @viewablePrepare@.
                        a -> -- ^ Отображаемые данные.
                        m ()
    viewableUnprepare _gui _p _a = return ()


-- | Контейнер с элементами отображаемыми с разной шириной.
class ViewableItems a p => ViewableVarWidthItems a p | a -> p where
    viewableGetWidth :: MonadIO m =>
                        Gui ->
                        p -> -- ^ Данные для отрисовки сформированные в @viewablePrepare@.
                        a -> -- ^ Отображаемые данные.
                        Int -> -- ^ Номер элемента.
                        Canvas m Coord -- ^ Видимая ширина элемента.

-- | Позиция горизонтального разделителя относительно списка элементов.
data HorizSepPos = FirstSeparator -- ^ Разделитель слева от первого элемента.
                 | MiddleSeparator -- ^ Обычный разделитель между элементами.
                 | LastSeparator -- ^ Разделитель справа от последнего элемента.
                 deriving Eq

-- | Позиция горизонтального разделителя относительно текущего элемента.
data HorizSepCurPos = OrdinalSeparator -- ^ Текущий элемент находится не рядом с разделителем.
                    | LeftIsCurItem  -- ^ Слева от разделителя находится текущий элемент.
                    | RightIsCurItem  -- ^ Справа от разделителя находится текущий элемент.
                    deriving Eq

data HorizSepPrepare p = HorizSepPrepare {
          horizSepPrFirstWidth :: Coord -- ^ Ширина разделителя типа FirstSeparator.
        , horizSepPrMiddleWidth :: Coord -- ^ Ширина разделителя типа MiddleSeparator.
        , horizSepPrLastWidth :: Coord -- ^ Ширина разделителя типа LastSeparator.
        , horizSepPrHeight :: Coord -- ^ Минимально возможная высота строки.
        , horizSepPrData :: p -- ^ Подготовленные данные для отрисовки разделителей.
                                         }

-- | Горизонтальный разделитель элементов.
class HorizontalSeparator a p | a -> p where
    horizSepPrepare :: MonadIO m =>
                       Gui ->
                       Skin ->
                       a -> -- ^ Исходные данные для настройки отображения разделителей.
                       Canvas m (HorizSepPrepare p)

    -- | Вызывается для отрисовки каждого разделителя.
    horizSepDrawItem :: MonadIO m =>
                        Gui ->
                        Skin ->
                        GuiRect -> -- ^ Координаты области для рисования.
                        p -> -- ^ Данные для отрисовки сформированные в @viewablePrepare@.
                        a -> -- ^ Исходные данные для настройки отображения разделителей.
                        HorizSepPos -> -- ^ Позиция разделителя относительно списка элементов.
                        HorizSepCurPos -> -- ^ Позиция разделителя относительно текущего элемента.
                        Canvas m ()

    -- | Вызывается после завершения использования для освобождения ресурсов выделенных во @horizSepPrepare@.
    horizSepUnprepare :: MonadIO m =>
                        Gui ->
                        p -> -- ^ Данные для отрисовки сформированные в @viewablePrepare@.
                        a -> -- ^ Исходные данные для настройки отображения разделителей.
                        m ()
    horizSepUnprepare _gui _p _a = return ()


-- | Вспомогательная функция для создания @instance ... ViewableItems@ с только текстовым отображением элементов.
viewablePrepareTextOnly :: MonadIO m => T.Text -> -- ^ Ключ шрифта
                                        Canvas m (Coord,Font)  -- ^ Минимально необходимая высота поля и шрифт
viewablePrepareTextOnly fontKey = do
    fnt <- getFont fontKey
    fntHeight <- FNT.lineSkip fnt -- FNT.height fnt
    return (fntHeight + 2 * PaddingY,fnt)

-- | Вспомогательная функция для создания @instance ... ViewableItems@ с отображением элемента одной строкой.
viewableDrawItemTextOnly :: (MonadIO m, DAROContainer c v, TS.TextShow v) =>
                        Font -> -- ^ Шрифт.
                        GuiRect -> -- ^ Координаты области для рисования.
                        DecoreState -> -- ^ Цвета фона и переднего плана.
                        c -> -- ^ Контейнер с данными.
                        Int -> -- ^ Номер элемента.
                        (v -> T.Text) -> -- ^ Функция преобразования элемента контейнера в 'Text'.
                        Canvas m ()
viewableDrawItemTextOnly fnt rect decoreState c ix f =
    drawTextAligned fnt AlignLeftCenter (decoreFgColor decoreState) (DrawStrOpaque (decoreBkColor decoreState))
        (shrinkRect (V2 PaddingX PaddingY) rect) . f =<< getItemDARO c ix

