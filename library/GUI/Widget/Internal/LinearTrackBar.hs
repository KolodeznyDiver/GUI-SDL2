{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module:      GUI.Widget.Internal.LinearTrackBar
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <kolodeznydiver@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Типы виджетов - линейных трекбаров. Есть два более гибких в настройке :
-- Горизонтальный hLinearTrackBar и вертикальный vLinearTrackBar, но оба определяются через одну slide функцию TH,
-- см. "GUI.Widget.TH.LinearTrackBar", т.к. имеют идентичную реализацию отличающуюся переменой координат осей.
-- А так же два полученных из них : горизонтальный hTrackBar и вертикальный vTrackBar, со встроенными функциями отрисовки,
-- см. "GUI.Widget.LinearTrackBar".

module GUI.Widget.Internal.LinearTrackBar(
    LinearTrackValueType,LinearTrackBarDef(..),LinearTrackBarStruct(..), LinearTrackBarData(..)
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Bits
import Data.IORef
import Data.Default
import GUI

-- | Тип, которым может манипулировать трекбар.
type LinearTrackValueType = Int

-- | Параметры настройки трекбара.
data LinearTrackBarDef = LinearTrackBarDef {
      -- | Общие настройки для всех виджетов для форм, в настоящий момент только margin's.
      linearTrackBarMargin  :: WidgetMargin
      -- | Для hLinearTrackBar,hTrackBar длина виджета, для vLinearTrackBar,vTrackBar высота.
    , linearTrackBarLn      :: Coord
      -- | Флаги базового виджета.
    , linearTrackBarFlags     :: WidgetFlags
      -- | Начальное минимальное значение. Пока виджет существует, можно изменять через @setMinValue@.
    , linearTrackMinValue :: LinearTrackValueType
      -- | Начальное миксимальное значение. Пока виджет существует, можно изменять через @setMaxValue@.
    , linearTrackMaxValue :: LinearTrackValueType
      -- | Начальное установленное значение. Пока виджет существует, можно изменять через @setValue@.
    , linearTrackBarPos      :: LinearTrackValueType
      -- | Для hLinearTrackBar,hTrackBar длина слайдера, для vLinearTrackBar,vTrackBar высота в пикселях.
      -- Если ==0, то подбирается автоматически.
    , linearTrackBarSliderLn :: Coord
      -- | Функция вызываемая для отрисовки виджета при настройках hLinearTrackBar,vLinearTrackBar.
      -- Для hTrackBar, vTrackBar оставить по умолчанию.
    , linearTrackBarDraw ::
        forall m. MonadIO m => Widget -> -- ^ трекбар
                               Maybe WidgetMouseState -> -- ^ Состояние мыши, см. 'GUI.Widget.Types.WidgetMouseState'.
                               GuiRect -> -- ^ Координаты трекбара.
                               Canvas m ()
      -- | Функция вызываемая для отрисовки слайдера виджета при настройках hLinearTrackBar,vLinearTrackBar.
      -- Для hTrackBar, vTrackBar оставить по умолчанию.
    , linearTrackBarSliderDraw ::
        forall m. MonadIO m => Widget -> -- ^ трекбар
                               Maybe WidgetMouseState -> -- ^ Состояние мыши, см. 'GUI.Widget.Types.WidgetMouseState'.
                               GuiRect -> -- ^ Координаты слайдера.
                               Canvas m ()
      -- | Функция для округления или другой модификации изменённого значения.
      -- По умолчанию просто возвращает аргумент.
    , linearTrackBarRounder :: forall m. MonadIO m => LinearTrackValueType -> m LinearTrackValueType
                                            }

instance Default LinearTrackBarDef where
    def = LinearTrackBarDef  { linearTrackBarMargin = WidgetMarginNone
                             , linearTrackBarLn = -1
                             , linearTrackBarFlags  = WidgetVisible .|. WidgetEnable
                             , linearTrackMinValue = 0
                             , linearTrackMaxValue = 1000
                             , linearTrackBarPos  = 0
                             , linearTrackBarSliderLn = 0
                             , linearTrackBarDraw = \ _ _ _ -> return ()
                             , linearTrackBarSliderDraw = \ _ _ _ -> return ()
                             , linearTrackBarRounder  = return
                             }

-- | Запись хранимая по ссылке для созданного виджета. Не используется за пределами
-- функций реализующих виджет и его свойства.
data LinearTrackBarStruct = LinearTrackBarStruct    { lnrTrBrMin :: LinearTrackValueType
                                                    , lnrTrBrMax :: LinearTrackValueType
                                                    , lnrTrBrVal :: LinearTrackValueType
                                                    , lnrTrBrOnChanged :: forall m. MonadIO m =>
                                                        LinearTrackValueType -> m ()
                                                    }

-- | Тип созданного виджета. Обычно используется как  @GuiWidget LinearTrackBarData@.
newtype LinearTrackBarData = LinearTrackBarData { getLnrTrBr :: IORef LinearTrackBarStruct }

-- | Позволяет установить обработчик на изменение контролируемого трекбаром параметра.
instance Changeable (GuiWidget LinearTrackBarData) LinearTrackValueType where
    onChanged w a = modifyMonadIORef' (getLnrTrBr $ getWidgetData w) (\d -> d{lnrTrBrOnChanged= a})

-- | Позволяет установить и получать минимальные и максимальные значения трекбара после его содания.
instance MinMaxValueProperty (GuiWidget LinearTrackBarData) LinearTrackValueType where
    setMinValue w v =  do
        a@LinearTrackBarStruct{..} <- readMonadIORef $ getLnrTrBr $ getWidgetData w
        when (lnrTrBrMin /= v) $ do
            writeMonadIORef (getLnrTrBr $ getWidgetData w) a{lnrTrBrMin= v}
            markWidgetForRedraw (getWidget w)
    getMinValue = fmap lnrTrBrMin . readMonadIORef . getLnrTrBr . getWidgetData
    setMaxValue w v = do
        a@LinearTrackBarStruct{..} <- readMonadIORef $ getLnrTrBr $ getWidgetData w
        when (lnrTrBrMax /= v) $ do
            writeMonadIORef (getLnrTrBr $ getWidgetData w) a{lnrTrBrMax= v}
            markWidgetForRedraw (getWidget w)
    getMaxValue = fmap lnrTrBrMax . readMonadIORef . getLnrTrBr . getWidgetData

-- | Позволяет установить и получать значение трекбара после его содания.
instance ValueProperty (GuiWidget LinearTrackBarData) LinearTrackValueType where
    setValue w v = do
        a@LinearTrackBarStruct{..} <- readMonadIORef $ getLnrTrBr $ getWidgetData w
        let newV = toBound lnrTrBrMin lnrTrBrMax v
        when ( newV/= lnrTrBrVal) $ do
            writeMonadIORef (getLnrTrBr $ getWidgetData w) a{lnrTrBrVal= newV}
            markWidgetForRedraw (getWidget w)
            lnrTrBrOnChanged newV
    getValue = fmap lnrTrBrVal . readMonadIORef . getLnrTrBr . getWidgetData
