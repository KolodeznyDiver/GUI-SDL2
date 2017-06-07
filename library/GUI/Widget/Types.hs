{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
module GUI.Widget.Types(
    pattern MinInsideSpaceX, pattern MinInsideSpaceY, pattern KbdClickSpecPoint
    ,WidgetMouseState(..),FormItemWidgetDef(..),NoArgAction(..),OneArgAction(..),OneArgPredicate(..)
    ,Clickable(..),Changeable(..)
    ,TextProperty(..),TextColorProperty(..),MinMaxValueProperty(..),ValueProperty(..),RowNumProperty(..)
    ,MouseStateProperty(..)
                        ) where
-- import Control.Monad
import Control.Monad.IO.Class -- (MonadIO)
import qualified Data.Text as T
--import Data.IORef
import Data.Default
--import qualified SDL
import SDL.Vect
import GUI.BaseLayer.Types
--import GUI.BaseLayer.Internal.Types
--import GUI.BaseLayer.Ref
--import GUI.BaseLayer.Widget

pattern MinInsideSpaceX :: Coord -- Минимальные расстояния между элементами внутри оснвных виджетов
pattern MinInsideSpaceX = 5
pattern MinInsideSpaceY :: Coord -- Минимальные расстояния между элементами внутри оснвных виджетов
pattern MinInsideSpaceY = 3

pattern KbdClickSpecPoint :: GuiPoint
pattern KbdClickSpecPoint = P (V2 (-1) (-1))

data WidgetMouseState = WidgetMouseOut | WidgetMouseIn | WidgetMousePressed
                 deriving (Eq, Show)

newtype FormItemWidgetDef = FormItemWidgetDef  { formItemMargin    :: Maybe WidgetMargin
                                            --, formItemAlignment :: Alignment
                                            }
                                            deriving (Show)

instance Default FormItemWidgetDef where
    def = FormItemWidgetDef Nothing

newtype NoArgAction = NoArgAction {noArgAction :: forall m. MonadIO m => m ()}

newtype OneArgAction a = OneArgAction {oneArgAction :: forall m. MonadIO m => a -> m ()}

newtype OneArgPredicate a = OneArgPredicate {oneArgPredicate :: forall m. MonadIO m => a -> m Bool}

class Clickable a where
    onClick :: forall m. MonadIO m => a -> (forall n. MonadIO n => n ()) -> m ()

class Changeable a b where
    onChanged :: forall m. MonadIO m => a -> (forall n. MonadIO n => b -> n ()) -> m ()

class TextProperty a where
    setText :: forall m. MonadIO m => a -> T.Text -> m ()
    getText :: forall m. MonadIO m => a -> m T.Text

class TextColorProperty a where
    setTextColor :: forall m. MonadIO m => a -> GuiColor -> m ()
    getTextColor :: forall m. MonadIO m => a -> m GuiColor

class MinMaxValueProperty a b where
    setMinValue :: forall m. MonadIO m => a -> b -> m ()
    getMinValue :: forall m. MonadIO m => a -> m b
    setMaxValue :: forall m. MonadIO m => a -> b -> m ()
    getMaxValue :: forall m. MonadIO m => a -> m b

class ValueProperty a b where
    setValue :: forall m. MonadIO m => a -> b -> m ()
    getValue :: forall m. MonadIO m => a -> m b

class RowNumProperty a where
    setRowNum :: forall m. MonadIO m => a -> Int -> m ()
    getRowNum :: forall m. MonadIO m => a -> m Int

class MouseStateProperty a where
    getMouseState :: forall m. MonadIO m => a -> m WidgetMouseState


