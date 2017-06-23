{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
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

mkLinearTrackBarQ DirectionH

mkLinearTrackBarQ DirectionV

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

hTrackBar :: MonadIO m => LinearTrackBarDef -> Widget -> Skin -> m (GuiWidget LinearTrackBarData)
hTrackBar = hvTrackBar hLinearTrackBar
{-# INLINE hTrackBar #-}

vTrackBar :: MonadIO m => LinearTrackBarDef -> Widget -> Skin -> m (GuiWidget LinearTrackBarData)
vTrackBar = hvTrackBar vLinearTrackBar
{-# INLINE vTrackBar #-}

