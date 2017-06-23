{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module GUI.Widget.Layout.Utils where

--import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.IORef
import Control.Monad.IO.Class (MonadIO)
import Data.Default
import SDL.Vect
import GUI
--import GUI.BaseLayer.Depend1.Geometry

data LayoutDef = LayoutDef  { layoutMargin    :: WidgetMargin
                            , layoutAlignment :: Alignment
                            , layoutColor     :: Maybe GuiColor
                            }

instance Default LayoutDef where
    def = LayoutDef WidgetMarginNone AlignCenter Nothing

newtype LinearLayoutData = LinearLayoutData { layoutSpaces :: IORef (VU.Vector GuiSize) }

linearLayoutCalc :: VU.Vector Coord -> Coord -> (VU.Vector Coord,Coord)
linearLayoutCalc childs allSpace =
    let cChilds = VU.length childs
        (sumS,sumK) = VU.foldl' (\(s,k) l -> if l < 0 then (s,k-l) else (s+l,k)) (0,0) childs
        freeS = allSpace - sumS
        freeK = (if (freeS>0) && (sumK>0) then negate $ fromIntegral freeS / fromIntegral sumK else 0) :: Double
    in (VU.generate cChilds (\i -> let l = childs VU.! i in
                                   if l < 0 then truncate $ fromIntegral l * freeK else l)
            ,if (freeS>0) && (sumK==0) then freeS else 0)


addToLinearLayout:: MonadIO m => GuiWidget LinearLayoutData -> (Widget -> Skin -> m (GuiWidget a)) -> m (GuiWidget a)
addToLinearLayout (GuiWidget widget LinearLayoutData{..}) initF = do
    modifyMonadIORef' layoutSpaces (`VU.snoc` zero)
    createWidget widget initF

instance WidgetComposer (GuiWidget LinearLayoutData) where
    ($+) = addToLinearLayout

maxSignedCoordFromVector :: VU.Vector Coord -> Coord
maxSignedCoordFromVector = VU.foldl' go 0
    where go r x | r>=0 && x>=0 = max r x
                 | r>=0 = x
                 | x>=0 = r
                 | otherwise = max r x