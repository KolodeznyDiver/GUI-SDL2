{-# LANGUAGE DeriveFunctor #-}
module GUI.BaseLayer.Types where

import qualified SDL
import SDL.Vect
import Foreign.C.Types
import Data.Word
import Data.Int
import Data.StateVar

type Coord = Int
type SDLCoord = CInt
type MouseCoord = Int32
type ColorComponent = Word8
type GuiTransparency = Word8
type GuiColor = V4 ColorComponent
type MousePoint = SDL.Point V2 MouseCoord
type GuiSize = V2 Coord
type GuiCoordOffset = V2 Coord
type GuiPoint = SDL.Point V2 Coord
type GuiRect = SDL.Rectangle Coord
type GuiWindowId = Word32
type GuiWindowIx = SDL.Window
type GuiCurDrawColor = StateVar GuiColor

data MarginLTRB a = MarginLTRB  { leftMargin :: a
                                , topMargin :: a
                                , rightMargin :: a
                                , bottomMargin :: a
                                }
                           deriving (Eq, Show,Functor)

type GuiMargin = MarginLTRB Coord


data WidgetMargin   = WidgetMarginNone
                    | WidgetMarginEvenly Coord
                    | WidgetMarginXY Coord Coord
                    | WidgetMarginLTRB Coord Coord Coord Coord
                    deriving (Eq, Show)

