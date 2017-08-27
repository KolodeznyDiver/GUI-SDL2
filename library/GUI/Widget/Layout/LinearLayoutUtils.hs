{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module:      GUI.Widget.Layout.LinearLayoutUtils
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <kolodeznydiver@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Типы и вспомгательные функции для реализации линейных layout-ов.
-- Линейные layout-ы бывают горизонтальные hLayout и вертикальные vLayout, но оба определяются
-- через одну slide (TH) функцию ,
-- см. "GUI.Widget.Layout.TH.LinearLayout", т.к. имеют идентичную реализацию отличающуюся переменой координат осей.

module GUI.Widget.Layout.LinearLayoutUtils where

import qualified Data.Vector.Unboxed as VU
import Data.IORef
import Data.Default
import SDL.Vect
import GUI

-- | Параметры настройки создаваемого layout-а.
data LayoutDef = LayoutDef {
      layoutMargin    :: WidgetMargin -- ^ Общие настройки для всех виджетов для форм,
                                      -- в настоящий момент только margin's.
    , layoutAlignment :: Alignment -- ^ Выравнивание элементов в layout-е.
    , layoutColor     :: Maybe GuiColor -- ^ Цвет фона layout-а или из 'Skin'.
                            }

instance Default LayoutDef where
    def = LayoutDef WidgetMarginNone AlignCenter Nothing

-- | Тип созданного layout-а. Обычно используется как  @GuiWidget LinearLayoutData@.
newtype LinearLayoutData = LinearLayoutData { layoutSpaces :: IORef (VU.Vector GuiSize) }

-- | Вспомогательная функция используемая в layout-ах для получения списка ширин
-- (или высот, в зависимости от типа лайаута) выделяемых элементам в лайауте.
linearLayoutCalc ::
    -- | Вектор одномерных размеров элементов. Если размер >=0, то считается что он такой и есть.
    -- а если он <0, то элемент претендует на выделение пространства оставшегося от остальных элементов,
    -- у которых размеры неотрицательные. Если, допустим, у одного элемнта размер (-1), а у другого (-3),
    -- то первому будет выделено 25% от оставшегося места, а второму 75%.
    VU.Vector Coord ->
    -- | полный размер в котором нужно распределить элементы.
    Coord ->
    -- | Возвращается исходный вектор в котором отрицательные значения заменены на выделенное им пространство.
    -- Вторым элементом кортежа возвращается оставшееся место, если элементов с отрицательными
    -- значениями в исходном векторе не было, или 0 иначе.
    (VU.Vector Coord,Coord)
linearLayoutCalc childs allSpace =
    let cChilds = VU.length childs
        (sumS,sumK) = VU.foldl' (\(s,k) l -> if l < 0 then (s,k-l) else (s+l,k)) (0,0) childs
        freeS = allSpace - sumS
        freeK = (if (freeS>0) && (sumK>0) then negate $ fromIntegral freeS / fromIntegral sumK else 0) :: Double
    in (VU.generate cChilds (\i -> let l = childs VU.! i in
                                   if l < 0 then truncate $ fromIntegral l * freeK else l)
            ,if (freeS>0) && (sumK==0) then freeS else 0)

-- | Реализация вставки виджета в линейный layout.
instance WidgetComposer (GuiWidget LinearLayoutData) where
    w $+ initF = modifyMonadIORef' (layoutSpaces $ widgetData w) (`VU.snoc` zero) >>
                   createWidget (baseWidget w) initF

-- | Вспомогательная функция используемая в layout-ах.
-- Если отрицательных элементов нет, то вернуть максимальное, иначе минимальное из отрицательных.
maxSignedCoordFromVector :: VU.Vector Coord -> Coord
maxSignedCoordFromVector = VU.foldl' go 0
    where go r x | r>=0 && x>=0 = max r x
                 | r>=0 = x
                 | x>=0 = r
                 | otherwise = min r x