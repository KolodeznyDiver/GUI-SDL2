{-# LANGUAGE PatternGuards #-}
module GUI.Utils.Wrap(
    rowWrapping
    ) where

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import SDL.Vect
import GUI.BaseLayer.Types
import GUI.BaseLayer.Geometry

-- | По переданному выравниванию, ширине области заполнения, межэлементному промежутку и вектору размеров элементов
-- | возвращает х координаты левых краёв элементов считая от области заполнения и мксимальные высоты элементов
-- | в каждой строке
rowWrapping :: HAlign -> Coord -> Coord -> VU.Vector GuiSize -> V.Vector (VU.Vector Coord,Coord)
rowWrapping align width xSpace v = byRow V.empty 0
    where cElem = VU.length v
          byElem xs maxH x i | Just (V2 w h) <- v VU.!? i =
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