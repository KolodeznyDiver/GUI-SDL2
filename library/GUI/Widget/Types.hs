{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module:      GUI.Widget.Types
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <kolodeznydiver@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Типы, классы и шаблоны для создания виджетов.

module GUI.Widget.Types(
    -- * Типы и константы используемые во многих виджетах.
    pattern MinInsideSpaceX, pattern MinInsideSpaceY, pattern KbdClickSpecPoint
    ,WidgetMouseState(..),FormItemWidgetDef(..)
    -- * Обёртки для полиморфных функций.
    ,NoArgAction(..),OneArgAction(..),OneArgPredicate(..)
    -- * Распрстранённые для виджетов динамические \"проперти\".
    ,Clickable(..),Changeable(..)
    ,TextProperty(..),TextColorProperty(..),MinMaxValueProperty(..),ValueProperty(..),RowNumProperty(..)
    ,MouseStateProperty(..)
                        ) where

import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.Default
import SDL.Vect
import GUI.BaseLayer.Depend0.Types

pattern MinInsideSpaceX :: Coord -- Минимальные расстояния между элементами внутри оснвных виджетов
pattern MinInsideSpaceX = 5
pattern MinInsideSpaceY :: Coord -- Минимальные расстояния между элементами внутри оснвных виджетов
pattern MinInsideSpaceY = 3

pattern KbdClickSpecPoint :: GuiPoint
pattern KbdClickSpecPoint = P (V2 (-1) (-1))

-- | Состояние мыши для виджетов отслеживающих мышь и анимирующих себя в зависимости от её положения.
data WidgetMouseState =
          WidgetMouseOut -- ^ Мышь не над виджетом.
        | WidgetMouseIn -- ^ Мышь над виджетом.
        | WidgetMousePressed -- ^ Мышь над виджетом и её кнопка нажата.
                 deriving (Eq, Show)

-- | Общие настройки для всех виджетов для форм, в настоящий момент только margin's.
newtype FormItemWidgetDef = FormItemWidgetDef { formItemMargin    :: Maybe WidgetMargin
                                              }
                                              deriving (Show)

instance Default FormItemWidgetDef where
    def = FormItemWidgetDef Nothing

-- | Некое действие без параметров. Реакция на какое то событие.
newtype NoArgAction = NoArgAction {noArgAction :: forall m. MonadIO m => m ()}

-- | Некое действие с одним параметров. Реакция на изменение чего то.
newtype OneArgAction a = OneArgAction {oneArgAction :: forall m. MonadIO m => a -> m ()}

-- | Некий предикат с одним параметров.
newtype OneArgPredicate a = OneArgPredicate {oneArgPredicate :: forall m. MonadIO m => a -> m Bool}

class Clickable a where
    onClick :: forall m. MonadIO m => a -> (forall n. MonadIO n => n ()) -> m ()

-- | Для экземпляров этого класса типов можно назначить действие на щелчёк.
class Changeable a b where
    onChanged :: forall m. MonadIO m => a -> (forall n. MonadIO n => b -> n ()) -> m ()

-- | Для экземпляров этого класса типов можно назначить установку и извлечение некоего текста.
class TextProperty a where
    setText :: forall m. MonadIO m => a -> T.Text -> m ()
    getText :: forall m. MonadIO m => a -> m T.Text

-- | Для экземпляров этого класса типов можно назначить установку и извлечение некоего цвета.
class TextColorProperty a where
    setTextColor :: forall m. MonadIO m => a -> GuiColor -> m ()
    getTextColor :: forall m. MonadIO m => a -> m GuiColor

-- | Для экземпляров этого класса типов можно назначить установку и извлечение некоего диапазона значений.
class MinMaxValueProperty a b where
    setMinValue :: forall m. MonadIO m => a -> b -> m ()
    getMinValue :: forall m. MonadIO m => a -> m b
    setMaxValue :: forall m. MonadIO m => a -> b -> m ()
    getMaxValue :: forall m. MonadIO m => a -> m b

-- | Для экземпляров этого класса типов можно назначить установку и извлечение некоего значения.
class ValueProperty a b where
    setValue :: forall m. MonadIO m => a -> b -> m ()
    getValue :: forall m. MonadIO m => a -> m b

-- | Для экземпляров этого класса типов можно назначить установку и извлечение номера некоего ряда.
class RowNumProperty a where
    setRowNum :: forall m. MonadIO m => a -> Int -> m ()
    getRowNum :: forall m. MonadIO m => a -> m Int

-- | Для экземпляров этого класса типов можно назначить извлечение состояние мыши.
class MouseStateProperty a where
    getMouseState :: forall m. MonadIO m => a -> m WidgetMouseState


