module GUI.BaseLayer.Color(rgb,grayColor) where

import SDL.Vect
import GUI.BaseLayer.Types

rgb :: ColorComponent -> ColorComponent -> ColorComponent -> GuiColor
rgb r g b = V4 r g b 0
{-# INLINE rgb #-}

grayColor :: ColorComponent -> GuiColor
grayColor w = V4 w w w 0
{-# INLINE grayColor #-}


