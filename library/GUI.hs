{-# LANGUAGE PatternSynonyms #-}
-- {-# LANGUAGE RankNTypes #-}
module GUI(
    -- GUI.BaseLayer.Types
    WidgetOpts,WindowOpts,WidgetFlags,WindowFlags,Widget,Window,Gui,Canvas
    ,GuiNotifyCode(..),WidgetFunctions(..),SpecStateWidget(..)
    ,WidgetRecord(..),WindowRecord(..),GUIRecord(..)
    -- GUI.BaseLayer.Depend0.Types
    ,Coord,SDLCoord,ColorComponent,GuiTransparency,GuiColor,MousePoint,GuiSize,GuiCoordOffset,GuiPoint,GuiRect
    ,GuiWindowId,GuiWindowIx,GuiCurDrawColor,MarginLTRB(..),GuiMargin,WidgetMargin(..)
    -- GUI.BaseLayer.GUIRecord
    ,guiGetLog,guiGetSkin,guiGetResourceManager
    -- GUI.BaseLayer.Window
    ,pattern WindowNoFlags,pattern WindowRedrawFlag,pattern WindowCloseOnLostFocuse,pattern WindowWaitAlt
    ,pattern WindowPopupFlag,pattern WindowLocked
    ,pattern WindowHaveKeyboardFocus, pattern WindowHaveMouseFocus,pattern WindowClickable
    ,getSDLWindow,getWindowRenderer,getWindowGui
    ,getWinId'',getWinId',getWinId,getWinIx',getWinIx
    ,removeWindowFlags,getWindowFlags,setWindowFlags,windowFlagsAddRemove,windowFlagsAdd
    ,windowFlagsRemove,allWindowFlags',allWindowFlags,anyWindowFlags,getWindowMainWidget
    ,getWindowForegroundWidget,getWindowsMap,getWindowByIx
    ,getFocusedWidget,setFocusedWidget,getWidgetUnderCursor,setWidgetUnderCursor
    ,getWinCursorIx,setWinCursorIx,doForWinByIx,allWindowsMap_,setWinMainMenu,getWinMainMenu
    -- GUI.BaseLayer.RedrawWindow
    ,showWinWidgets
    -- GUI.Widget
    ,pattern WidgetNoFlags, pattern WidgetRedrawFlag, pattern WidgetSelectable, pattern WidgetEnable
    ,pattern WidgetVisible, pattern WidgetFocusable, pattern WidgetTabbed, pattern WidgetMouseWheelControl
    ,pattern WidgetFocused
    ,GuiWidget(..),getWidget,getWidgetData,getWidgetParent
    ,getWidgetRect,setWidgetRect,getWidgetCanvasRect,setWidgetCanvasRect,getWidgetVisibleRect
    ,getVisibleRect
    ,getWidgetMargin,setWidgetMargin,setWidgetMargin'
    ,getWidgetMarginSize,setWidgetRectWithMarginShrink
    ,getWidgetRectWithMargin,calcWidgetSizeWithMargin
    ,widgetCoordsToStr,showWidgets,showWidgetsFromMain
    ,getWidgetFlags,setWidgetFlags,widgetFlagsAddRemove
    ,widgetFlagsAdd,widgetFlagsRemove,allWidgetFlags',allWidgetFlags,anyWidgetFlags
    ,setWidgetFns,getWidgetFns,getWidgetParentFns,widgetResizingIfChanged
    ,setWidgetCursorIx,getWidgetCursorIx,getWidgetWindow
    ,getWidgetChildrenCount,getWidgetChild
    ,foldByWidgetChildren,foldByWidgetChildren',ifoldByWidgetChildren,ifoldByWidgetChildren'
    ,foldByWidgetChildren_,foldByWidgetChildren'_,ifoldByWidgetChildren_,ifoldByWidgetChildren'_
    ,mapByWidgetChildren,imapByWidgetChildren,mapByWidgetChildren_,imapByWidgetChildren_,forEachWidgets
    ,getChildWidgetIx,getWidgetParentIx,getPrevNextWidgets,isMainWidget,getWinMainWidget
    ,findWidgetInTreeForward,findWidgetInTreeBackward,findNextTabbedWidget,findPrevTabbedWidget
    -- GUI.Utils
    ,WidgetComposer(..),newForeground,moveWidget,resizeWidget,resizeWidgetWithCanvas
    ,mulDiv,getWidgetCoordOffset,coordToWidget,mouseToWidget
    ,runProxyWinCanvas,runProxyCanvas,getSkinFromWin,getSkinFromWidget
    ,guiSetCursor,setGuiState,getGuiState,notifyEachWidgetsInAllWin
    ,redrawAll,forEachWidgetsInAllWin,createWidget,mkWidget,SimpleWidget(..),mkSimpleWidget
    ,delWidget,delAllChildWidgets,lastChildReplaceFirst
    ,delWindowByIx,delWindow,delAllWindows,delWindowsBy,windowsFold,getWindowsCount,delAllPopupWindows
    ,markWidgetForRedraw,setWidgetFlag,notifyParentAboutSize,simpleOnResizing
    ,extendableOnResizing,enableWidget,visibleWidget,newWindow',newWindow,newModalWindow',newModalWindow
    ,overlapsChildrenFns,setWinCursorIx,setWinSize
    ,guiApplicationExitSuccess,guiApplicationExitFailure,guiApplicationExitWithCode
    ,logPutLn,guiOnSomeException,logOnErr
    -- GUI.BaseLayer.Pipe
    ,GuiPipeId,GuiPipeProducer,GuiPipe(newGuiPipe,sendToGuiPipe,replaceGuiPipeHandler),getPipeIdFromProducer,delGuiPipe
    -- GUI.BaseLayer.Depend0.Cursor
    ,CursorIx(..),pattern DefCursorIx,freeCursor
    -- GUI.BaseLayer.Depend1.Skin
    ,Border3DColors(..),BtnBorderType(..),DecoreState(..),ButtonDecore(..),Skin(..)
    -- GUI.BaseLayer.Depend0.Ref
    ,newMonadIORef,readMonadIORef,writeMonadIORef,modifyMonadIORef' -- ,atomicallyMonadIO,newTVarMonadIO,readTVarMonadIO
    -- GUI.BaseLayer.Depend1.Geometry
    ,Alignment(..),VAlign(..),HAlign(..),DirectionVH(..)
    ,xV2,yV2,getRectLT,sizeOfRect,hvAlignToAlignment,getVAlign,getHAlign,vAlignToOff,hAlignToOff
    ,rectToBriefStr,directionLetter,rectAlign,toBound,isInRect,rectIntersection,getRectLB,getRectRT,getRectRB
    ,rectCenter,isEmptyRect,marginSize,rectShrinkByMargin,rectGrowByMargin,marginToLTRB,marginToBriefStr
    ,moveRect,moveRectTo,rectMove,shrinkRect,shrinkRect',mkDotLineVector,sizeReplaceIfNoPositive,sizeRestoreNegative
    ,mkDotRectVector
    -- GUI.BaseLayer.Depend1.Color
    ,rgb,grayColor
    -- GUI.BaseLayer.Depend0.TTF
    ,GuiFontStyle(..)
    -- GUI.BaseLayer.Depend1.Resource
    ,GuiFontOptions(..),GuiFontDef(..),ResourceManager
    -- GUI.BaseLayer.Resource
    ,initResourceManager,destroyResourceManager,rmGetSurfaceFromCache,rmGetSurface,rmAddSurface
    ,rmGetFont,rmLoadFont,rmGetCursor,rmSetCursor,rmAddCursor
    -- GUI.BaseLayer.Primitives
    ,DrawStrMode(..)
    -- GUI.BaseLayer.Canvas
    ,Orientation(..)
    ,toCanvasPoint,toCanvasRect,toSDLPoint,toSDLRect
    ,drawStretchedTexture,drawTexturePartial,drawTexture,drawTextureAligned,drawTextureEx
    ,drawLine,drawLines,drawPoint,drawPoints,drawRect,drawRects,fillRect,fillRects
    ,getTexture
    ,drawStretchedTextureR,drawTexturePartialR,drawTextureR,drawTextureAlignedR,drawTextureExR
    ,createTargetTexture,setBlendMode,setColor,withBlendMode,withColor,withTargetTexture,withClipRect,getFont
    ,withTransparentTexture,getStrSize
    ,renderStr,renderStrDraft,renderStrOpaque,drawStr,drawStrDraft,drawStrOpaque,drawStrAligned
    ,getTextSize,renderText,renderTextDraft,renderTextOpaque,drawText,drawTextDraft,drawTextOpaque,drawTextAligned
    ,drawRoundBorder,drawRoundFrame,draw3DBorder,draw3DFrame,drawDotBorder,drawArrowTriangle,drawArrow
    -- GUI.Widget.Types
    ,pattern MinInsideSpaceX, pattern MinInsideSpaceY, pattern KbdClickSpecPoint
    ,WidgetMouseState(..),FormItemWidgetDef(..),NoArgAction(..),OneArgAction(..),OneArgPredicate(..)
    ,Clickable(..),Changeable(..)
    ,TextProperty(..),TextColorProperty(..),MinMaxValueProperty(..),ValueProperty(..),RowNumProperty(..)
    ,MouseStateProperty(..)
    -- GUI.BaseLayer.Action
    ,pattern AllInvisibleActionMask,pattern AllDisableActionMask,pattern AllEnableActionMask
    ,ActionMask(..),GuiState(..),ActionValue(..),Action(..),Actions
    ,ActionState(..),ActionFn
    ,kNoModifier,kCtrl,kShift,kAlt,kCtrlShift,kCtrlAlt,kShiftAlt,getActionValueState,isVisibleActionState
    ,mkActionMask,mkEmptyActions
    ,addActions,chkHotKey,getActionByGroupAndId,getActionsByGroupAndId,setActionEnable
    ,setAction,getVisibleActions,fromActionMask
    -- GUI.BaseLayer.Depend0.Keyboard
    ,ShiftCtrlAlt(..),getShftCtrlAlt,isEnterKey,showbKeycode,KeyModifiers(..),KeyWithModifiers(..)
    ,scaToKeyModifier
    -- GUI.BaseLayer.Depend1.Logging
    ,LogDateTime(..),GUILogDef(..)
    -- GUI.BaseLayer.Depend0.Auxiliaries
    ,getAppName,mkRandomWinTitle,showErrMsgBoxT,showErrMsgBoxTL,showErrMsgBoxB,showErrMsgBox
    -- GUI.BaseLayer.UniqueCode
    ,UniqueCode(..),getUniqueCode
    -- GUI.BaseLayer.SpecStateWidget
    ,setMouseCapturedWidget,getMouseCapturedWidget,resetMouseCaptured,resetMouseCapturedWidget
    -- GUI.BaseLayer.Focus
    ,clearFocusInWindow,clearWidgetFocus,setWidgetFocus
    -- GUI.BaseLayer.RunGUI
    ,GUIDef(..),runGUI
  ) where

import GUI.BaseLayer.Depend0.Types
import GUI.BaseLayer.Depend0.Auxiliaries
import GUI.BaseLayer.Depend0.Cursor
import GUI.BaseLayer.Depend0.Keyboard
import GUI.BaseLayer.Depend0.Ref
import GUI.BaseLayer.Depend1.Color
import GUI.BaseLayer.Depend1.Geometry
import GUI.BaseLayer.Depend1.Skin
import GUI.BaseLayer.Depend1.Logging (LogDateTime(..),GUILogDef(..))
import GUI.BaseLayer.Types
import GUI.BaseLayer.Widget
import GUI.BaseLayer.Window
import GUI.BaseLayer.Pipe
import GUI.BaseLayer.Resource
import GUI.BaseLayer.Canvas
import GUI.BaseLayer.Core
import GUI.BaseLayer.RunGUI
import GUI.BaseLayer.Action
import GUI.BaseLayer.UniqueCode
import GUI.Widget.Types
import GUI.BaseLayer.RedrawWindow
import GUI.BaseLayer.GUIRecord
import GUI.BaseLayer.SpecStateWidget
import GUI.BaseLayer.Focus