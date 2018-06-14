{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      GUI.Widget.Container.Border
-- Copyright:   (c) 2017-2018 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- @border@ отчасти выполняет роль декоративного виджета позволяющего создавать на форме
-- различные, рамки, полоски, линии, прямоугольники. Возможно, с заголовками.
--
-- Однако, border, так же является и контейнером виджетов, т.е в него можно вставить другой виджет,
-- как в рамку (название frame было после некоторых размышлений отклонено).
-- Можно вставить и несколько виджетов которые будут занимать одно и то же место.
-- Понятно, что если только один дочерний виджет сделать видимым, то можно получить, в будущем,
-- область с закладками.

module GUI.Widget.Container.Border(
    BorderBackground(..),BorderType(..),BorderDef(..),BorderData,border
    ) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.Bits
import Data.Maybe
import Control.Monad.Extra (whenJust)
import Data.Default
import qualified SDL
import SDL.Vect
import GUI

-- | Отступ заголовка от края виджета.
pattern CaptionPaddingX :: Coord
pattern CaptionPaddingX = 5

-- | Настройка фона.
data BorderBackground = BorderTransparent -- ^ Прозрачный.
                      | BorderBkColorOfSkin -- ^ взять цвет из @decoreBkColor (formDecore skin)@.
                      | BorderBkColor GuiColor -- ^ Свой цвет.
                      deriving Eq

-- | Тип представления  border-а.
data BorderType = BorderMono  -- ^ Просто прямоуольник равномерно заполненный цветом.
                | BorderRect  -- ^ Тонкая рамка (в 1 пиксель).
                -- | Рамка с закруглёнными краями.
                | BorderRound
                -- | Трёхмерная рамка. Плохо смотрится вместе с заголовоком.
                | Border3D  {border3DLightColor :: Maybe GuiColor
                            ,border3DDarkColor :: Maybe GuiColor
                            }
                -- | Рамка из точек заданного шага.
                | BorderDot {borderDotStep :: Coord }
                -- | Линия. Вертикальная или горизонтальная определяется по соотношению сторон.
                | BorderLine
                deriving Eq

-- | Все параметры border-а.
data BorderDef = BorderDef  {
      borderFormItemDef  :: FormItemWidgetDef  -- ^ Общие настройки для всех виджетов для форм
                                               -- в настоящий момент только margin's.
    , borderSize      :: GuiSize -- ^ Размер без полей.
    , borderFlags     :: WidgetFlags -- ^ Флаги базового виджета.
    , borderType      :: BorderType -- ^ См. выше.
    , borderBkgrnd :: BorderBackground -- ^ См. выше.
    , borderThickness :: Coord -- ^ Указывает не только ширину 3D рамки, но, так же определяет область
                               -- занимаемую дочерними виджетами.
    , borderCaption :: T.Text -- ^ Заголовок отображаемый на рамке. T.empty если он не нужен (по умолчанию).
    , borderCaptionColor :: Maybe GuiColor -- ^ Цвет рамки или прямоугольника если не устраивает из 'Skin'.
    , borderCaptionAlignment :: HAlign -- ^ Выравнивание заголовка на верхней границе рамки.
    , borderFontKey :: T.Text -- ^ Ключ шрифта заголовка. По умолчанию \"small\".
    , borderFgColor :: Maybe GuiColor -- ^ Цвет заголовка если не устраивает из 'Skin'.
    , borderOnlyOneChild :: Bool -- ^ Разрешать добавлять только один дочерний виджет.
                                 -- Если есть предыдущий - он удаляется.
    , borderSizeByChild :: Bool -- ^ Если @False@ - border пытается подогнать под свой размер
                                -- дочерние виджеты. Если True - наоборот.
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

-- | Тип созданного border-а. В отличии  от большинста виджетов он не имеет IORef-ов.
-- как и все подобные типы экспортируется только левая часть.
data BorderData = BorderData {brdbrDataOnlyOneChild :: Bool}

-- | Функция создания бордера.
border :: MonadIO m => BorderDef ->  -- ^ Параметры виджета.
                       Widget -> -- ^ Будующий предок в дереве виджетов.
                       Skin -> -- ^ Skin.
                       m (GuiWidget BorderData)
border BorderDef{..} parent skin = do
    let bkgrndColor = case borderBkgrnd of
                        BorderBkColor c -> c
                        _ -> decoreBkColor (formDecore skin)
        frgrndColor = fromMaybe (formBorderColor skin) borderFgColor
        isTransp = borderBkgrnd==BorderTransparent
        fns = overlapsChildrenFns borderSize
    (mbFnt,topSpacing) <-
        if not $ T.null borderCaption then runProxyCanvas parent $ do
            fnt <- getFont borderFontKey
            V2 _ captionH <- getTextSize fnt borderCaption
            return (Just fnt,max borderThickness captionH)
        else return (Nothing,borderThickness)
    let shrinkByBorder = rectShrinkByMargin (MarginLTRB borderThickness topSpacing borderThickness borderThickness)
    mkWidget borderFlags
            (fromMaybe (formItemsMargin skin) $ formItemMargin borderFormItemDef)
            (BorderData borderOnlyOneChild) parent fns{
        onDraw= \widget -> do
            r@(SDL.Rectangle (P (V2 x0 y0)) (V2 w h)) <- getVisibleRect widget
            let bkFill = unless isTransp (setColor bkgrndColor >> fillRect r)
                r' | isJust mbFnt = let dy = topSpacing `div` 2 in SDL.Rectangle (P (V2 x0 (y0+dy))) (V2 w (h-dy))
                   | otherwise = r
                lightClr = fromMaybe (brdr3DLightColor $ brdr3DColors skin)
            case borderType of
              BorderMono -> bkFill
              BorderRect -> bkFill >> setColor frgrndColor >> drawRect r'
              BorderRound | isTransp -> setColor frgrndColor >> drawRoundBorder r'
                          | otherwise -> drawRoundFrame frgrndColor bkgrndColor r'
              Border3D{..} -> bkFill >> draw3DBorder (lightClr border3DLightColor)
                                            (fromMaybe (brdr3DDarkColor $ brdr3DColors skin) border3DDarkColor)
                                            borderThickness r
              BorderDot{..} -> bkFill >> setColor frgrndColor >> drawDotBorder borderDotStep r'
              BorderLine -> do
                bkFill
                let (p0,p1) | w>h = let y= y0 + h `div` 2 in (V2 x0 y, V2 (x0+w) y)
                            | otherwise = let x= x0 + w `div` 2 in (V2 x y0, V2 x (y0+h))
                setColor frgrndColor
                drawLine (P p0) (P p1)
--            liftIO $ putStrLn $ concat ["label.onDraw getVisibleRect=", rectToBriefStr r]
            whenJust mbFnt $ \fnt ->
                let bkClr = case borderType of
                                Border3D{..} -> DrawStrOpaque (lightClr border3DLightColor)
                                _ | isTransp -> DrawStrFine
                                  | otherwise -> DrawStrOpaque bkgrndColor
                in drawTextAligned fnt (hvAlignToAlignment borderCaptionAlignment VTop) frgrndColor bkClr
                    (shrinkRect (V2 CaptionPaddingX 0) r) $ T.cons ' ' $ T.snoc borderCaption ' '
        ,onSizeChangedParentNotify= \widget child newSz ->
            if borderSizeByChild then do
                widgetResizingIfChanged child $ SDL.Rectangle zero newSz
                maxSz <- foldByWidgetChildren' (\sz widg -> ((\x -> max <$> sz <*> x) . sizeOfRect) <$>
                            getWidgetRectWithMargin widg) zero widget
                resizeWidgetWithCanvas widget $ (V2 (borderThickness*2) (topSpacing+borderThickness)) ^+^ maxSz
            else onSizeChangedParentNotify fns widget child newSz
        ,onResizing= \widget newRect -> do
                r <- extendableOnResizing borderSize widget newRect
                unless borderSizeByChild $ do
                    let r' = -- shrinkRect' borderThickness
                            shrinkByBorder (SDL.Rectangle zero (sizeOfRect r))
                    mapByWidgetChildren_ (\c -> do {fs <- getWidgetFns c; onResizing fs c r'}) widget
                                }

-- | Реализация вставки виджета в border.
instance WidgetComposer (GuiWidget BorderData) where
    (GuiWidget widget BorderData{..}) $+ initF = do
        when brdbrDataOnlyOneChild $ delAllChildWidgets widget
        r@(GuiWidget child _) <- createWidget widget initF
        unless brdbrDataOnlyOneChild $ do
            cCnt <- getWidgetChildrenCount widget
            when (cCnt>1) $ widgetFlagsRemove child $ WidgetVisible .|. WidgetRedrawFlag
        return r