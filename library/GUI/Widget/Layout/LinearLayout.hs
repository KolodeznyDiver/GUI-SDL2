{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module GUI.Widget.Layout.LinearLayout(
    -- GUI.Utils
    LinearLayoutData,addToLinearLayout
    -- GUI.Utils.LinearLayout
    ,LayoutDef(..),vLayout,hLayout
                         ) where

import GUI
import GUI.Widget.Layout.Utils
import GUI.Widget.Layout.TH.LinearLayout

mkLinearLayoutQ DirectionH

mkLinearLayoutQ DirectionV
