{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
module GUI(
    -- GUI.BaseLayer.Internal.Types
    WidgetOpts,WindowOpts,WidgetFlags,GuiWidgetFlags,WindowFlags,Widget,GuiWindow,Gui,Canvas,GuiCanvas,
    WidgetFunctions(..)
    -- GUI.BaseLayer.Types
    ,Coord,GuiSize,MouseCoord,ColorComponent,GuiColor,MousePoint,GuiPoint,GuiRect,GuiWindowId,GuiCurDrawColor
    ,WidgetMargin(..)
    -- GUI.BaseLayer.Window
    ,pattern WindowNoFlags,pattern WindowCloseOnLostFocuse
    ,getWinId'',getWinId',getWinId,getWinIx',getWinIx
    ,removeWindowFlags,getWindowFlags,setWindowFlags,windowFlagsAddRemove,windowFlagsAdd
    ,windowFlagsRemove,allWindowFlags',allWindowFlags,anyWindowFlags
    ,getSDLWindow,getWindowRenderer
    ,getWindowByIx,getFocusedWidget,setFocusedWidget,getWidgetUnderCursor,setWidgetUnderCursor
    ,getWinCursorIx,setWinCursorIx
    ,showWinWidgets,newWindow,getGuiFromWindow,getWindowMainWidget,getWindowsMap
    ,doForWinByIx,allWindowsMap_,redrawWindowByIx,redrawWindow,isSpecStateWidget
    ,resetSpecStateWidget,setSpecStateWidget,setMouseCapturedWidget,getMouseCapturedWidget
    ,resetMouseCaptured,resetMouseCapturedWidget,setWinMainMenu,getWinMainMenu
    -- GUI.Widget
    ,pattern WidgetNoFlags, pattern WidgetSelectable, pattern WidgetEnable, pattern WidgetVisible
    ,pattern WidgetFocusable,pattern WidgetTabbed, pattern WidgetMouseWheelControl,pattern WidgetFocused
--    ,WidgetInit(..)
    ,GuiWidget(..),defWidgFns,getWidget,getWidgetData,getWidgetParent
    ,setWidgetParent,getWidgetRect,setWidgetRect,getWidgetCanvasRect,setWidgetCanvasRect,getWidgetVisibleRect
    ,getVisibleRect
    ,getWidgetMargin,setWidgetMargin,setWidgetMargin'
    ,getWidgetMarginSize,setWidgetRectWithMarginShrink,setWidgetRectWithMarginShrinkMoveOnly
    ,getWidgetRectWithMargin,calcWidgetSizeWithMargin,notifyParentSizeWithMargin
    ,simpleOnResizing,simpleOnResizingMoveOnly,widgetCoordsToStr,showWidgets,showWidgetsFromMain
    --,toWidgetCanvasCoord,toCanvasCoord,getWidgetFsAndCoord
    ,getWidgetFlags,setWidgetFlags,widgetFlagsAddRemove
    ,widgetFlagsAdd,widgetFlagsRemove,allWidgetFlags,anyWidgetFlags
    ,setWidgetFns,getWidgetFns,getWidgetParentFns,widgetResizingIfChanged
    ,setWidgetCursorIx,getWidgetCursorIx,getWidgetWindow
    ,markWidgetForRedraw,setWidgetFlag,enableWidget,visibleWidget,getWidgetChildrenCount,getWidgetChild
    ,foldByWidgetChildren,foldByWidgetChildren',ifoldByWidgetChildren,ifoldByWidgetChildren'
    ,foldByWidgetChildren_,foldByWidgetChildren'_,ifoldByWidgetChildren_,ifoldByWidgetChildren'_
    ,mapByWidgetChildren,imapByWidgetChildren,mapByWidgetChildren_,imapByWidgetChildren_
    ,getChildWidgetIx,getWidgetParentIx,getPrevWidget,isMainWidget,getWinMainWidget,mkWidget'
    -- GUI.Utils
    ,WidgetComposer(..),mulDiv,getWidgetCoordOffset,coordToWidget,mouseToWidget
    ,runProxyWinCanvas,runProxyCanvas,guiGetSkin,getSkinFromWin,getSkinFromWidget,guiGetResourceManager
    ,guiSetCursor,getShftCtrlAlt,isEnterKey,clearFocusInWindow,clearWidgetFocus,setWidgetFocus
    ,redrawAll,createWidget,mkWidget,SimpleWidget(..),mkSimpleWidget
    ,delWidget,delAllChildWidgets,lastChildReplaceFirst
    ,delWindowByIx,delWindow,delAllWindows
    -- GUI.BaseLayer.Event
    ,RedrawRequestMode(..),redrawRequestByWinId,redrawRequestByWidget
    -- GUI.BaseLayer.Cursor
    ,GuiCursor(..),CursorIx(..),pattern DefCursorIx,getCursor,setCursor
    ,activeCursor,freeCursor,createSystemCursor,CursorSet(..),freeCursorSet,mkSystemCursorSet,cursorFromSystemIx
    -- GUI.BaseLayer.Skin
    ,BorderType(..),DecoreState(..),ButtonDecore(..),Skin(..)
    -- GUI.BaseLayer.Ref
    ,newMonadIORef,readMonadIORef,writeMonadIORef,modifyMonadIORef',atomicallyMonadIO,newTVarMonadIO,readTVarMonadIO
    -- GUI.BaseLayer.Geometry
    ,Alignment(..),VAlign(..),HAlign(..),DirectionVH(..)
    ,xV2,yV2,pointOfRect,sizeOfRect,getVAlign,getHAlign,vAlignToOff,hAlignToOff
    ,rectToBriefStr,directionLetter,rectAlign,toBound,isInRect,rectIntersection,getRectLB,getRectRT,getRectRB
    ,rectCenter,isEmptyRect,marginSize,rectShrinkByMargin,rectGrowByMargin,marginToLTRB,marginToLT,marginToBriefStr
    ,moveRect,moveRectTo,rectMove,shrinkRect,shrinkRect',mkDotLineVector,mkDotRectVector
    -- GUI.BaseLayer.Color
    ,rgb,grayColor
    -- GUI.BaseLayer.Resource.Types
    ,GuiFontDef(..),ResourceManager
    -- GUI.BaseLayer.Resource
    ,initResourceManager,destroyResourceManager,rmGetTexture,rmAddTexture
    ,rmGetFont,rmLoadFont,rmGetCursor,rmSetCursor,rmAddCursor
    -- GUI.BaseLayer.Primitives
    ,DrawStrMode(..)
    -- GUI.BaseLayer.Canvas
    ,Orientation(..)
    ,toCanvasPoint,toCanvasRect,toSDLPoint,toSDLRect,runCanvas
    ,drawStretchedTexture,drawTexturePartial,drawTexture,drawTextureAligned,drawTextureEx
    ,drawLine,drawLines,drawPoint,drawPoints,drawRect,drawRects,fillRect,fillRects
    ,getTexture,getTextureFromCache
    ,drawStretchedTextureR,drawTexturePartialR,drawTextureR,drawTextureAlignedR,drawTextureExR
    ,createTargetTexture,setBlendMode,setColor,withBlendMode,withColor,withTargetTexture,withClipRect,getFont
    ,withTransparentTexture,getStrSize,
    renderStr,renderStrDraft,renderStrOpaque,drawStr,drawStrDraft,drawStrOpaque,drawStrAligned
    ,getTextSize,renderText,renderTextDraft,renderTextOpaque,drawText,drawTextDraft,drawTextOpaque,drawTextAligned
    ,drawRoundBorder,drawRoundFrame,draw3DBorder,draw3DFrame,drawDotBorder,drawArrowTriangle,drawArrow
    -- GUI.BaseLayer.Handlers
    ,noChildrenFns,noChildrenMoveOnlyFns,oneChildFns
    -- GUI.Widget.Types
    ,pattern MinInsideSpaceX, pattern MinInsideSpaceY, pattern KbdClickSpecPoint
    ,WidgetMouseState(..),FormItemWidgetDef(..),NoArgAction(..),OneArgAction(..),Clickable(..),Changeable(..)
    ,TextProperty(..),TextColorProperty(..),MinMaxValueProperty(..),ValueProperty(..),RowNumProperty(..)
    ,MouseStateProperty(..)
    -- GUI.BaseLayer.GUI
    ,runGUI
  ) where

import GUI.BaseLayer.Types
import GUI.BaseLayer.Ref
import GUI.BaseLayer.Internal.Types
import GUI.BaseLayer.Widget
import GUI.BaseLayer.Window
import GUI.BaseLayer.Event
import GUI.BaseLayer.Resource
import GUI.BaseLayer.Geometry
import GUI.BaseLayer.Canvas
import GUI.BaseLayer.Skin
import GUI.BaseLayer.Cursor
import GUI.BaseLayer.Utils
import GUI.BaseLayer.GUI
import GUI.BaseLayer.Color
import GUI.BaseLayer.Handlers
import GUI.Widget.Types