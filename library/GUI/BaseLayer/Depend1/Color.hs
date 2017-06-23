-- |
-- Module:      GUI.BaseLayer.Depend1.Color
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <kolodeznydiver@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Функции цветокодирования.

module GUI.BaseLayer.Depend1.Color(rgb,grayColor) where

import SDL.Vect
import GUI.BaseLayer.Depend0.Types

-- | Задаёт цвет по трём компонентам R,G,B без прозрачности
rgb :: ColorComponent -> ColorComponent -> ColorComponent -> GuiColor
rgb r g b = V4 r g b 0
{-# INLINE rgb #-}

-- | Задаёт цвет из серой шкалы по коду 0..255
grayColor :: ColorComponent -> GuiColor
grayColor w = V4 w w w 0
{-# INLINE grayColor #-}


