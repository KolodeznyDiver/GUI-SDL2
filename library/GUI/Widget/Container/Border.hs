{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module GUI.Widget.Container.Border(
    BorderBackground(..),BorderType(..),BorderDef(..),BorderData,border
    ) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.Bits
import Data.Maybe
import Maybes (whenIsJust)
import Data.Default
import qualified SDL
import SDL.Vect
import GUI

pattern CaptionPaddingX :: Coord
pattern CaptionPaddingX = 5

data BorderBackground = BorderTransparent
                      | BorderBkColorOfSkin
                      | BorderBkColor GuiColor
                      deriving Eq

data BorderType = BorderMono
                | BorderRect
                | BorderRound { borderRoundOutsideColor :: Maybe GuiColor
                              }
                | Border3D  {border3DLightColor :: Maybe GuiColor
                            ,border3DDarkColor :: Maybe GuiColor
                            }
                | BorderDot {borderDotStep :: Coord }
                | BorderLine
                deriving Eq

data BorderDef = BorderDef  { borderFormItemDef  :: FormItemWidgetDef
                            , borderSize      :: GuiSize
                            , borderFlags     :: WidgetFlags
                            , borderType      :: BorderType
                            , borderBkgrnd :: BorderBackground
                            , borderThickness :: Coord
                            , borderCaption :: T.Text
                            , borderCaptionColor :: Maybe GuiColor
                            , borderCaptionAlignment :: HAlign
                            , borderFontKey :: T.Text
                            , borderFgColor :: Maybe GuiColor
                            , borderOnlyOneChild :: Bool
                            , borderSizeByChild :: Bool
                            }

instance Default BorderDef where
    def = BorderDef  { borderFormItemDef = def
                     , borderSize = zero
                     , borderFlags = WidgetVisible
                     , borderType = BorderLine
                     , borderBkgrnd = BorderBkColorOfSkin
                     , borderThickness = 2
                     , borderCaption = T.empty
                     , borderCaptionColor = Nothing
                     , borderCaptionAlignment = HLeft
                     , borderFontKey = "small"
                     , borderFgColor = Nothing
                     , borderOnlyOneChild = True
                     , borderSizeByChild = False
                     }

data BorderData = BorderData {brdbrDataOnlyOneChild :: Bool}

border :: MonadIO m => BorderDef -> Widget -> Skin -> m (GuiWidget BorderData)
border BorderDef{..} parent skin = do
    let bkgrndColor = case borderBkgrnd of
                        BorderBkColor c -> c
                        _ -> bkColor skin
        frgrndColor = fromMaybe (borderColor skin) borderFgColor
        isTransp = borderBkgrnd==BorderTransparent
        fns = overlapsChildrenFns borderSize
    mbFnt <- if not $ T.null borderCaption
             then Just <$> runProxyCanvas parent (getFont borderFontKey) else return Nothing
    mkWidget borderFlags
            (fromMaybe (formItemsMargin skin) $ formItemMargin borderFormItemDef)
            (BorderData borderOnlyOneChild) parent fns{
        onDraw= \widget -> do
            r@(SDL.Rectangle (P (V2 x0 y0)) (V2 w h)) <- getVisibleRect widget
            let bkFill = unless isTransp (setColor bkgrndColor >> fillRect r)
                r' | isJust mbFnt = let dy = borderThickness `div` 2 in SDL.Rectangle (P (V2 x0 (y0+dy))) (V2 w (h-dy))
                   | otherwise = r
                lightClr = fromMaybe (brdr3DLightColor skin)
            case borderType of
              BorderMono -> bkFill
              BorderRect -> bkFill >> setColor frgrndColor >> drawRect r'
              BorderRound{..} | isTransp -> setColor frgrndColor >> drawRoundBorder r'
                              | otherwise -> drawRoundFrame (fromMaybe bkgrndColor borderRoundOutsideColor)
                                                 frgrndColor bkgrndColor r'
              Border3D{..} -> bkFill >> draw3DBorder (lightClr border3DLightColor)
                                         (fromMaybe (brdr3DDarkColor skin) border3DDarkColor) borderThickness r
              BorderDot{..} -> bkFill >> setColor frgrndColor >> drawDotBorder borderDotStep r'
              BorderLine -> do
                bkFill
                let (p0,p1) | w>h = let y= y0 + h `div` 2 in (V2 x0 y, V2 (x0+w) y)
                            | otherwise = let x= x0 + w `div` 2 in (V2 x y0, V2 x (y0+h))
                setColor frgrndColor
                drawLine (P p0) (P p1)
--            liftIO $ putStrLn $ concat ["label.onDraw getVisibleRect=", rectToBriefStr r]
            whenIsJust mbFnt $ \fnt ->
                let bkClr = case borderType of
                                Border3D{..} -> DrawStrOpaque (lightClr border3DLightColor)
                                _ | isTransp -> DrawStrFine
                                  | otherwise -> DrawStrOpaque bkgrndColor
                in drawStrAligned fnt (hvAlignToAlignment borderCaptionAlignment VTop) frgrndColor bkClr
                    (shrinkRect (V2 CaptionPaddingX 0) r) $ T.unpack $ T.cons ' ' $ T.snoc borderCaption ' '
        ,onSizeChangedParentNotify= \widget child newSz ->
            if borderSizeByChild then do
                widgetResizingIfChanged child $ SDL.Rectangle zero newSz
                maxSz <- foldByWidgetChildren' (\sz widg -> ((\x -> max <$> sz <*> x) . sizeOfRect) <$>
                            getWidgetRectWithMargin widg) zero widget
                resizeWidgetWithCanvas widget $ (let x = borderThickness*2 in V2 x x) ^+^ maxSz
            else onSizeChangedParentNotify fns widget child newSz
        ,onResizing= \widget newRect -> do
                r <- extendableOnResizing borderSize widget newRect
                unless borderSizeByChild $ do
                    let r' = shrinkRect' borderThickness (SDL.Rectangle zero (sizeOfRect r))
                    mapByWidgetChildren_ (\c -> do {fs <- getWidgetFns c; onResizing fs c r'}) widget
                                }

instance WidgetComposer (GuiWidget BorderData) where
    (GuiWidget widget BorderData{..}) $+ initF = do
        when brdbrDataOnlyOneChild $ delAllChildWidgets widget
        r@(GuiWidget child _) <- createWidget widget initF
        unless brdbrDataOnlyOneChild $ do
            cCnt <- getWidgetChildrenCount widget
            when (cCnt>1) $ widgetFlagsRemove child $ WidgetVisible .|. WidgetRedrawFlag
        return r