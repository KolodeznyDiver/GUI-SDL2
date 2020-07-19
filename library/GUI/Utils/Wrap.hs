{-# LANGUAGE PatternGuards #-}
-- |
-- Module:      GUI.Utils.Wrap
-- Copyright:   (c) 2017-2020 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Вспомогательная функция для расчёта укладки прямоугольных элементов в пространстве заданной ширины рядами.
--
-- Подходит как для размещения текста с переносом строк. Тогда исходные области, это размеры непробельных
-- фрагментов текста.
--
-- Так же подходит для размещения виджетов в плавающем лайауте. Тогда исходные размеры, это размеры виджетов с полями.


module GUI.Utils.Wrap(
    rowWrapping
    ) where

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import SDL.Vect
import GUI.BaseLayer.Depend0.Types
import GUI.BaseLayer.Depend1.Geometry

-- | Вспомогательная функция для расчёта укладки прямоугольных элементов в пространстве заданной ширины рядами.
-- По переданному выравниванию, ширине области заполнения, межэлементному промежутку и вектору размеров элементов
-- | возвращает х координаты левых краёв элементов считая от области заполнения и мксимальные высоты элементов
-- | в каждой строке
rowWrapping ::
    -- | Горизонтальное выравнивание.
    HAlign ->
    -- | Ширина области заполнения.
    Coord ->
    -- | Горизонтальный интервал требуемый между элементами.
    Coord ->
    -- | 'V2' размеры укладываемых элементов. Особый случай @V2 maxBound _@ указывает принудительный переход на
    -- новую строку. Такой элемент не попадает в выходную последовательность.
    VU.Vector GuiSize ->
    -- | Вектор сформированных описаний строк размещения элементов. Каждая трока описывается вектором левых границ
    -- элементов в строке и максимальной высостой строки.
    V.Vector (VU.Vector Coord,Coord)
rowWrapping align width xSpace v = byRow V.empty 0
    where cElem = VU.length v
          byElem xs maxH x i | Just (V2 w h) <- v VU.!? i =
                                -- Особый случай : команда переноса строки
                                if w == maxBound then (xs,maxH,width - x,i+1) else
                                    let pos = if x == 0 then 0 else x + xSpace
                                        x' = pos + w in
                                    if x' > width then (xs,maxH,width - x,i)
                                    else byElem (xs `VU.snoc` pos) (max maxH h) x' $ i + 1
                             | otherwise = (xs,maxH,width - x,i)
          byRow r i | i == cElem = r
                    | otherwise = case v VU.!? i of
                                    Just (V2 w h)
                                        | w>=width -> byRow (r `V.snoc` (VU.singleton 0,h)) $ i+1
                                        | otherwise -> let (xs,maxH,rest,i') = byElem VU.empty 0 0 i
                                                           off = hAlignToOff align rest
                                                       in byRow (r `V.snoc` (VU.map (off+) xs,maxH)) i'
                                    _ -> r