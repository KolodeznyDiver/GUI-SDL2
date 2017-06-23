{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

-- |
-- Module:      GUI.BaseLayer.Depend0.Types
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <kolodeznydiver@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Определения основных, не взаимозависящих, типов GUI.

module GUI.BaseLayer.Depend0.Types(
    -- * Координаты на экране.
    Coord,SDLCoord,MousePoint,GuiSize,GuiCoordOffset,GuiPoint,GuiRect
    -- * Коды цвета и прозрачности.
    ,ColorComponent,GuiTransparency,GuiColor,GuiCurDrawColor
    -- * Margin - размеры отступов виджета от границы которую он занимает до области где он
    -- может рисовать и от которой отсчитываются его внутренние координаты.
    ,MarginLTRB(..),GuiMargin,WidgetMargin(..)
    -- * Типы идентифицирующие конкретное окно.
    ,GuiWindowId,GuiWindowIx
    ) where

import Foreign.C.Types
import Data.Word
import Data.Int
import GHC.Generics (Generic)
import Data.Data (Data)
import Data.Typeable
import Data.StateVar
import qualified SDL
import SDL.Vect

-- Координаты

-- | Координата на экране в пикселях. Основной тип координат в GUI.
type Coord = Int

-- | Координата на экране для большинства функций SDL и некоторых SDL сообщений.
type SDLCoord = CInt

-- | Координата на экране для сообщений от мыши (так уж в SDL).
type MouseCoord = Int32

-- | 2D расстояние, размер или дистаниция на экране в GUI. Но не точка на экране. Точка - @GuiPoint@.
type GuiSize = V2 Coord

-- | 2D смещение координат на экране относительно друг друга.
type GuiCoordOffset = V2 Coord

-- | 2D Точка на экране в координатах или Widget-a, или окна.
type GuiPoint = SDL.Point V2 Coord

-- | 2D Точка на экране в SDL сообщениях от мыши.
type MousePoint = SDL.Point V2 MouseCoord

-- | Координаты прямоугольника на экране, см. 'SDL.Rectangle'.
type GuiRect = SDL.Rectangle Coord

-- Цвета

-- | Один из трёх компонентов цвета R,G или B. В SDL цвет задаётся вектором V4 r g b a,
-- хотя прозрачность __a__ обычно не учитывается, кроме специальной настройки, которая в GUI
-- может быть использована применением функции @GUI.BaseLayer.Canvas.withTransparentTexture@.
type ColorComponent = Word8

-- | Прозрачность, задаваемая в @GUI.BaseLayer.Canvas.withTransparentTexture@.
type GuiTransparency = Word8

-- | Полное описание цвета R,G,B,A.
type GuiColor = V4 ColorComponent

-- | @StateVar@ тип для получения и установки текущего цвета для конкретного окна.
type GuiCurDrawColor = StateVar GuiColor

-- Margin - размеры отступов виджета от границы которую он занимает до области где он
-- может рисовать и от которой отсчитываются его внутренние координаты.

-- | Тип, которым задаются отступы при создании виджета.
data WidgetMargin   = WidgetMarginNone           -- ^ Отступов нет
                    | WidgetMarginEvenly Coord   -- ^ Со всех сторон одинаковые отступы
                    | WidgetMarginXY Coord Coord -- ^ Отступ слева=справа и сверху=снизу
                    | WidgetMarginLTRB           -- ^ Отступы слева, сверху, справа, снизу.
                        Coord Coord Coord Coord
                    deriving (Show, Read, Data, Eq, Ord, Generic, Typeable)


-- | В этот тип преобразуется @WidgetMargin@ при создании виджета.
data MarginLTRB a = MarginLTRB  { leftMargin :: a
                                , topMargin :: a
                                , rightMargin :: a
                                , bottomMargin :: a
                                }
                           deriving (Eq, Show,Functor)

-- | В таком виде отступы хранятся в структуре @GUI.BaseLayer.Types.WidgetRecord@
-- описывающей виджет.
type GuiMargin = MarginLTRB Coord

-- Типы идентифицирующие конкретное окно.

-- | Уникальный идентификатор окна в SDL.
type GuiWindowId = Word32

-- | Тип самого окна в SDL.
type GuiWindowIx = SDL.Window





