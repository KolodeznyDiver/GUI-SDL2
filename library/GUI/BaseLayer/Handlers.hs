{-# LANGUAGE RankNTypes #-}
-- {- DuplicateRecordFields -}
-- {- RecordWildCards -}
module GUI.BaseLayer.Handlers(
    noChildrenFns,noChildrenMoveOnlyFns
        ) where

import Control.Monad
import GUI.BaseLayer.Types
import GUI.BaseLayer.Widget
import GUI.BaseLayer.Internal.Types
import GUI.BaseLayer.Utils

noChildrenFns :: GuiSize -> WidgetFunctions
noChildrenFns initInsideSz = defWidgFns{
    onCreate = \widget -> notifyParentAboutSize widget initInsideSz
    ,onResizing= \widget -> void . extendableOnResizing initInsideSz widget
                             }
{-# INLINE noChildrenFns #-}

noChildrenMoveOnlyFns :: GuiSize -> WidgetFunctions
noChildrenMoveOnlyFns initInsideSz = (noChildrenFns initInsideSz){
    onResizing= \widget -> void . simpleOnResizingMoveOnly widget
                             }
{-# INLINE noChildrenMoveOnlyFns #-}

