{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
module GUI(
    -- GUI.BaseLayer.Internal.Types
    WidgetOpts,WindowOpts,WidgetFlags,WindowFlags,Widget,GuiWindow,Gui,Canvas(..),GuiCanvas
    ,UniqueCode(..),GuiNotifyCode(..),GuiPipeId(..),WidgetFunctions(..),SpecStateWidget(..)
    ,GuiWidgetCollection,GuiWindowCollection,UserMsgHandler(..),GuiPipeCollection
    ,WidgetStruct(..),WindowStruct(..),GUIStruct(..)
    -- GUI.BaseLayer.Types
    ,Coord,SDLCoord,ColorComponent,GuiTransparency,GuiColor,MousePoint,GuiSize,GuiCoordOffset,GuiPoint,GuiRect
    ,GuiWindowId,GuiWindowIx,GuiCurDrawColor,MarginLTRB(..),GuiMargin,WidgetMargin(..)
    -- GUI.BaseLayer.Window
    ,pattern WindowNoFlags,pattern WindowCloseOnLostFocuse,pattern WindowPopupFlag,pattern WindowClickable
    ,getWinId'',getWinId',getWinId,getWinIx',getWinIx
    ,removeWindowFlags,getWindowFlags,setWindowFlags,windowFlagsAddRemove,windowFlagsAdd
    ,windowFlagsRemove,allWindowFlags',allWindowFlags,anyWindowFlags
    ,getSDLWindow,getWindowRenderer
    ,getWindowByIx,getFocusedWidget,setFocusedWidget,getWidgetUnderCursor,setWidgetUnderCursor
    ,getWinCursorIx
    ,showWinWidgets,getGuiFromWindow,getWindowMainWidget,getWindowForegroundWidget,getWindowsMap
    ,doForWinByIx,allWindowsMap_,redrawWindowByIx,redrawWindow,isSpecStateWidget
    ,resetSpecStateWidget,setSpecStateWidget,setMouseCapturedWidget,getMouseCapturedWidget
    ,resetMouseCaptured,resetMouseCapturedWidget,setWinMainMenu,getWinMainMenu
    -- GUI.Widget
    ,pattern WidgetNoFlags, pattern WidgetRedrawFlag, pattern WidgetSelectable, pattern WidgetEnable
    ,pattern WidgetVisible, pattern WidgetFocusable, pattern WidgetTabbed, pattern WidgetMouseWheelControl
    ,pattern WidgetFocused
    ,GuiWidget(..),defWidgFns,getWidget,getWidgetData,getWidgetParent
    ,setWidgetParent,getWidgetRect,setWidgetRect,getWidgetCanvasRect,setWidgetCanvasRect,getWidgetVisibleRect
    ,getVisibleRect
    ,getWidgetMargin,setWidgetMargin,setWidgetMargin'
    ,getWidgetMarginSize,setWidgetRectWithMarginShrink,setWidgetRectWithMarginShrinkMoveOnly
    ,getWidgetRectWithMargin,calcWidgetSizeWithMargin
    ,widgetCoordsToStr,showWidgets,showWidgetsFromMain
    --,toWidgetCanvasCoord,toCanvasCoord,getWidgetFsAndCoord
    ,getWidgetFlags,setWidgetFlags,widgetFlagsAddRemove
    ,widgetFlagsAdd,widgetFlagsRemove,allWidgetFlags',allWidgetFlags,anyWidgetFlags
    ,setWidgetFns,getWidgetFns,getWidgetParentFns,widgetResizingIfChanged
    ,setWidgetCursorIx,getWidgetCursorIx,getWidgetWindow
    ,getWidgetChildrenCount,getWidgetChild
    ,foldByWidgetChildren,foldByWidgetChildren',ifoldByWidgetChildren,ifoldByWidgetChildren'
    ,foldByWidgetChildren_,foldByWidgetChildren'_,ifoldByWidgetChildren_,ifoldByWidgetChildren'_
    ,mapByWidgetChildren,imapByWidgetChildren,mapByWidgetChildren_,imapByWidgetChildren_,forEachWidgets
    ,getChildWidgetIx,getWidgetParentIx,getPrevNextWidgets,isMainWidget,getWinMainWidget
    ,findWidgetInTreeForward,findWidgetInTreeBackward,findNextTabbedWidget,findPrevTabbedWidget,mkWidget'
    -- GUI.Utils
    ,WidgetComposer(..),newForeground,moveWidget,resizeWidget,resizeWidgetWithCanvas
    ,mulDiv,getWidgetCoordOffset,coordToWidget,mouseToWidget
    ,runProxyWinCanvas,runProxyCanvas,guiGetSkin,getSkinFromWin,getSkinFromWidget,guiGetResourceManager
    ,guiSetCursor,setGuiState,getGuiState,getUniqueCode,notifyEachWidgetsInAllWin
    ,clearFocusInWindow,clearWidgetFocus,setWidgetFocus
    ,redrawAll,forEachWidgetsInAllWin,createWidget,mkWidget,SimpleWidget(..),mkSimpleWidget
    ,delWidget,delAllChildWidgets,lastChildReplaceFirst
    ,delWindowByIx,delWindow,delAllWindows,delWindowsBy,windowsFold,getWindowsCount,delAllPopupWindows
    ,markWidgetForRedraw,setWidgetFlag,notifyParentAboutSize,simpleOnResizing,simpleOnResizingMoveOnly
    ,extendableOnResizing,enableWidget,visibleWidget,newWindow',newWindow,overlapsChildrenFns,setWinCursorIx,setWinSize
    ,guiApplicationExitSuccess,guiApplicationExitFailure,guiApplicationExitWithCode
    -- GUI.BaseLayer.Event
    ,GuiPipeProducer,GuiPipe(newGuiPipe,sendToGuiPipe,replaceGuiPipeHandler),getPipeIdFromProducer,delGuiPipe
    -- GUI.BaseLayer.Cursor
    ,GuiCursor(..),CursorIx(..),pattern DefCursorIx,getCursor,setCursor
    ,activeCursor,freeCursor,createSystemCursor,CursorSet(..),freeCursorSet,mkSystemCursorSet,cursorFromSystemIx
    -- GUI.BaseLayer.Skin
    ,BtnBorderType(..),DecoreState(..),ButtonDecore(..),Skin(..)
    -- GUI.BaseLayer.Ref
    ,newMonadIORef,readMonadIORef,writeMonadIORef,modifyMonadIORef' -- ,atomicallyMonadIO,newTVarMonadIO,readTVarMonadIO
    -- GUI.BaseLayer.Geometry
    ,Alignment(..),VAlign(..),HAlign(..),DirectionVH(..)
    ,xV2,yV2,pointOfRect,sizeOfRect,hvAlignToAlignment,getVAlign,getHAlign,vAlignToOff,hAlignToOff
    ,rectToBriefStr,directionLetter,rectAlign,toBound,isInRect,rectIntersection,getRectLB,getRectRT,getRectRB
    ,rectCenter,isEmptyRect,marginSize,rectShrinkByMargin,rectGrowByMargin,marginToLTRB,marginToLT,marginToBriefStr
    ,moveRect,moveRectTo,rectMove,shrinkRect,shrinkRect',mkDotLineVector,sizeReplaceIfNoPositive,sizeRestoreNegative
    ,mkDotRectVector
    -- GUI.BaseLayer.Color
    ,rgb,grayColor
    -- GUI.BaseLayer.Raw.TTF
    ,GuiFontStyle(..)
    -- GUI.BaseLayer.Resource.Types
    ,GuiFontOptions(..),GuiFontDef(..),ResourceManager
    -- GUI.BaseLayer.Resource
    ,initResourceManager,destroyResourceManager,rmGetSurfaceFromCache,rmGetSurface,rmAddSurface
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
    ,withTransparentTexture,getStrSize
    ,renderStr,renderStrDraft,renderStrOpaque,drawStr,drawStrDraft,drawStrOpaque,drawStrAligned
    ,getTextSize,renderText,renderTextDraft,renderTextOpaque,drawText,drawTextDraft,drawTextOpaque,drawTextAligned
    ,drawRoundBorder,drawRoundFrame,draw3DBorder,draw3DFrame,drawDotBorder,drawArrowTriangle,drawArrow
    -- GUI.BaseLayer.Handlers
    ,noChildrenFns,noChildrenMoveOnlyFns
    -- GUI.Widget.Types
    ,pattern MinInsideSpaceX, pattern MinInsideSpaceY, pattern KbdClickSpecPoint
    ,WidgetMouseState(..),FormItemWidgetDef(..),NoArgAction(..),OneArgAction(..),OneArgPredicate(..)
    ,Clickable(..),Changeable(..)
    ,TextProperty(..),TextColorProperty(..),MinMaxValueProperty(..),ValueProperty(..),RowNumProperty(..)
    ,MouseStateProperty(..)
    -- GUI.BaseLayer.Action
    ,pattern AllInvisibleActionMask,pattern AllDisableActionMask,pattern AllEnableActionMask
    ,HotKeyModifier(..),HotKey(..),ActionMask(..),GuiState(..),ActionValue(..),Action(..),Actions
    ,ActionState(..),ActionId(..),ActionFn
    ,hkNoModifier,hkCtrl,hkShift,hkAlt,hkCtrlShift,hkCtrlAlt,hkShiftAlt,getActionValueState,isVisibleActionState
    ,mkActionMask,mkEmptyActions
    ,scaToHotKeyModifier,addActions,chkHotKey,getActionByGroupAndId,getActionsByGroupAndId,setActionEnable
    ,setAction,getVisibleActions,fromActionMask
    -- GUI.BaseLayer.Keyboard
    ,ShiftCtrlAlt(..),getShftCtrlAlt,isEnterKey,showKeycode
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
import GUI.BaseLayer.Action
import GUI.BaseLayer.Keyboard
import GUI.Widget.Types