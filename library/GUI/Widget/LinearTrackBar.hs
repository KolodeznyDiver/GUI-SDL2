{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
-- |
-- Module:      GUI.Widget.LinearTrackBar
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <kolodeznydiver@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Линейные трекбары. Есть два более гибких в настройке :
-- Горизонтальный hLinearTrackBar и вертикальный vLinearTrackBar, но оба определяются через одну slide функцию TH,
-- см. "GUI.Widget.TH.LinearTrackBar", т.к. имеют идентичную реализацию отличающуюся переменой координат осей.
-- А так же два полученных из них : горизонтальный hTrackBar и вертикальный vTrackBar, со встроенными функциями отрисовки,
-- см. "GUI.Widget.LinearTrackBar".

module GUI.Widget.LinearTrackBar(
    -- GUI.Widget.Internal.LinearTrackBar
    LinearTrackValueType,LinearTrackBarDef(..),LinearTrackBarData
    -- GUI.Widget.LinearTrackBar
    ,hLinearTrackBar,vLinearTrackBar,hTrackBar,vTrackBar
                                   ) where

import Control.Monad.IO.Class
import GUI
import GUI.Widget.TH.LinearTrackBar
import GUI.Widget.Internal.LinearTrackBar
import GUI.Widget.Button (getButtonDecoreState,drawButtonFrame)

-- | Генерация функции @hLinearTrackBar@.
mkLinearTrackBarQ DirectionH

-- | Генерация функции @vLinearTrackBar@.
mkLinearTrackBarQ DirectionV

-- | Не экспортируемая вспомогательная функция для создания упрощённых трекбаров с отрисовкой по умолчанию.
hvTrackBar :: forall m. MonadIO m =>
    (LinearTrackBarDef -> Widget -> Skin -> m (GuiWidget LinearTrackBarData)) ->
    LinearTrackBarDef -> Widget -> Skin -> m (GuiWidget LinearTrackBarData)
hvTrackBar f dat parent skin@Skin{..} = f dat{
        linearTrackBarDraw = \ _ _ r -> do
            setColor scrollBarColor
            fillRect r
        , linearTrackBarSliderDraw = \ _ mbSt r -> do
            let decoreSt = getButtonDecoreState mbSt scrollBarSlider
            drawButtonFrame decoreSt (btnDecoreBorder scrollBarSlider) scrollBarColor r
                       } parent skin

-- | Горизонтальный трекбар с отрисовкой по умолчанию.
hTrackBar :: MonadIO m => LinearTrackBarDef -> Widget -> Skin -> m (GuiWidget LinearTrackBarData)
hTrackBar = hvTrackBar hLinearTrackBar
{-# INLINE hTrackBar #-}

-- | Вертикальный трекбар с отрисовкой по умолчанию.
vTrackBar :: MonadIO m => LinearTrackBarDef -> Widget -> Skin -> m (GuiWidget LinearTrackBarData)
vTrackBar = hvTrackBar vLinearTrackBar
{-# INLINE vTrackBar #-}

