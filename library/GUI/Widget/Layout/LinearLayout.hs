{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
-- |
-- Module:      GUI.Widget.Layout.LinearLayout
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <kolodeznydiver@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Линейные layout-ы бывают горизонтальные hLayout и вертикальные vLayout, но оба определяются
-- через одну slide (TH) функцию ,
-- см. "GUI.Widget.Layout.TH.LinearLayout", т.к. имеют идентичную реализацию отличающуюся переменой координат осей.

module GUI.Widget.Layout.LinearLayout(
    -- GUI.Utils
    LinearLayoutData
    -- GUI.Utils.LinearLayout
    ,LayoutDef(..)
    -- GUI.Widget.Layout.LinearLayout
    ,hLayout,vLayout
                         ) where

import GUI
import GUI.Widget.Layout.LinearLayoutUtils
import GUI.Widget.Layout.TH.LinearLayout

-- | Сгенерировать функцию @hLayout@.
mkLinearLayoutQ DirectionH

-- | Сгенерировать функцию @vLayout@.
mkLinearLayoutQ DirectionV
