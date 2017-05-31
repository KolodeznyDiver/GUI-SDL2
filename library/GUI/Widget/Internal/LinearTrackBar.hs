{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module GUI.Widget.Internal.LinearTrackBar(
    LinearTrackValueType,LinearTrackBarDef(..),LinearTrackBarStruct(..), LinearTrackBarData(..)
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Bits
import Data.IORef
import Data.Default
import GUI
import GUI.Widget.Handlers

type LinearTrackValueType = Int

data LinearTrackBarDef = LinearTrackBarDef  {
                            linearTrackBarMargin  :: WidgetMargin
                          , linearTrackBarLn      :: Coord
                          , linearTrackBarFlags     :: WidgetFlags
                          , linearTrackMinValue :: LinearTrackValueType
                          , linearTrackMaxValue :: LinearTrackValueType
                          , linearTrackBarPos      :: LinearTrackValueType
                          , linearTrackBarSliderLn :: Coord -- 0 - пропорционально соотношению отображаемого окна
                          , linearTrackBarDraw :: forall m. MonadIO m =>
                                Widget -> Maybe WidgetMouseState -> GuiRect -> GuiCanvas m ()
                          , linearTrackBarSliderDraw :: forall m. MonadIO m =>
                                Widget -> Maybe WidgetMouseState -> GuiRect -> GuiCanvas m ()
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

data LinearTrackBarStruct = LinearTrackBarStruct    { lnrTrBrMin :: LinearTrackValueType
                                                    , lnrTrBrMax :: LinearTrackValueType
                                                    , lnrTrBrVal :: LinearTrackValueType
                                                    , lnrTrBrOnChanged :: OneArgAction LinearTrackValueType
                                                    }

newtype LinearTrackBarData = LinearTrackBarData { getLnrTrBr :: IORef LinearTrackBarStruct }

instance Changeable (GuiWidget LinearTrackBarData) LinearTrackValueType where
    onChanged w a = modifyMonadIORef' (getLnrTrBr $ getWidgetData w) (\d -> d{lnrTrBrOnChanged=OneArgAction a})

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

instance ValueProperty (GuiWidget LinearTrackBarData) LinearTrackValueType where
    setValue w v = do
        a@LinearTrackBarStruct{..} <- readMonadIORef $ getLnrTrBr $ getWidgetData w
        let newV = toBound lnrTrBrMin lnrTrBrMax v
        when ( newV/= lnrTrBrVal) $ do
            writeMonadIORef (getLnrTrBr $ getWidgetData w) a{lnrTrBrVal= newV}
            markWidgetForRedraw (getWidget w)
            oneArgAction lnrTrBrOnChanged newV
    getValue = fmap lnrTrBrVal . readMonadIORef . getLnrTrBr . getWidgetData
