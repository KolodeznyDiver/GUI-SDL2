{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
-- |
-- Module:      GUI.Widget.LinearTrackBar
-- Copyright:   (c) 2017-2020 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
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
    ,hLinearTrackBar,vLinearTrackBar,hTrackBar',hTrackBar,vTrackBar',vTrackBar,setLinearTrackBarSliderLn
                                   ) where

import Control.Monad
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
    LinearTrackBarDef ->
    (LinearTrackBarDef -> LinearTrackBarDef) -> -- ^ функция изменения LinearTrackBarDef после установки обработчиков отрисовки.
    Widget ->
    Skin ->
    m (GuiWidget LinearTrackBarData)
hvTrackBar f dat defPostF parent skin@Skin{..} = f (defPostF dat{
          linearTrackBarDraw = \ _ _ r -> do
            setColor trackBarBkColor
            fillRect r
        , linearTrackBarSliderDraw = \ _ mbSt r -> do
            let decoreSt = getButtonDecoreState mbSt trackBarSlider
            drawButtonFrame decoreSt (btnDecoreBorder trackBarSlider) r
                                             }) parent skin

-- | Горизонтальный трекбар с отрисовкой по умолчанию.
hTrackBar' :: MonadIO m =>
              LinearTrackBarDef ->
              (LinearTrackBarDef -> LinearTrackBarDef) -> -- ^ функция изменения LinearTrackBarDef после установки обработчиков отрисовки.
              Widget ->
              Skin ->
              m (GuiWidget LinearTrackBarData)
hTrackBar' = hvTrackBar hLinearTrackBar
{-# INLINE hTrackBar' #-}

-- | Горизонтальный трекбар с отрисовкой по умолчанию.
hTrackBar :: MonadIO m => LinearTrackBarDef -> Widget -> Skin -> m (GuiWidget LinearTrackBarData)
hTrackBar d = hTrackBar' d id
{-# INLINE hTrackBar #-}

-- | Вертикальный трекбар с отрисовкой по умолчанию.
vTrackBar' :: MonadIO m =>
              LinearTrackBarDef ->
              (LinearTrackBarDef -> LinearTrackBarDef) -> -- ^ функция изменения LinearTrackBarDef после установки обработчиков отрисовки.
              Widget ->
              Skin ->
              m (GuiWidget LinearTrackBarData)
vTrackBar' = hvTrackBar vLinearTrackBar
{-# INLINE vTrackBar' #-}

-- | Вертикальный трекбар с отрисовкой по умолчанию.
vTrackBar :: MonadIO m => LinearTrackBarDef -> Widget -> Skin -> m (GuiWidget LinearTrackBarData)
vTrackBar d = vTrackBar' d id
{-# INLINE vTrackBar #-}

-- | Установить новую длину ползунка в пикселях.
setLinearTrackBarSliderLn :: MonadIO m => GuiWidget LinearTrackBarData -> Double -> m ()
setLinearTrackBarSliderLn w v = do
        let widg   = baseWidget w
            wStruc = getLnrTrBr $ widgetData w
        a@LinearTrackBarStruct{..} <- readMonadIORef wStruc
        when ( v > 0 && v /= lnrTrBrSliderLn) $ do
            writeMonadIORef wStruc a{lnrTrBrSliderLn= v}
            markWidgetForRedraw widg