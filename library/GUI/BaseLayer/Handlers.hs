{-# LANGUAGE RankNTypes #-}
-- {- DuplicateRecordFields -}
-- {- RecordWildCards -}
module GUI.BaseLayer.Handlers(
    noChildrenFns,noChildrenMoveOnlyFns,oneChildFns
        ) where

import Control.Monad
import GUI.BaseLayer.Types
import GUI.BaseLayer.Widget
import GUI.BaseLayer.Internal.Types

noChildrenFns :: GuiSize -> WidgetFunctions
noChildrenFns initInsideSz = defWidgFns{
    onCreate = \widget -> notifyParentSizeWithMargin widget initInsideSz
    ,onResizing= \widget -> void . simpleOnResizing widget
                             }
{-# INLINE noChildrenFns #-}

noChildrenMoveOnlyFns :: GuiSize -> WidgetFunctions
noChildrenMoveOnlyFns initInsideSz = (noChildrenFns initInsideSz){
    onResizing= \widget -> void . simpleOnResizingMoveOnly widget
                             }
{-# INLINE noChildrenMoveOnlyFns #-}

oneChildFns :: WidgetFunctions
oneChildFns = defWidgFns{
     onSizeChangedParentNotiy= \widget child _ -> getWidgetCanvasRect widget >>= widgetResizingIfChanged child
    ,onResizing= \widget newRect -> do
                r <- simpleOnResizing widget newRect
                mapByWidgetChildren_ (\c -> do {fs <- getWidgetFns c; onResizing fs c r}) widget
                             }
