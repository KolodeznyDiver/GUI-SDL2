{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module:      GUI
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Модуль реэкспортирующий типы и функции из /GUI.BaseLayer/ и из "GUI.Widget.Types" которые,
-- предположительно, будут использованы при построении виджетов верхнего у ровня и в коде приложения.
--
-- Для использования конкретных виджетов, разумеется, кроме
--
-- > import GUI
--
-- требуется импортировать ещё и модули кокретных виджетов.

module GUI(
     module GUI.BaseLayer.Depend0.BitFlags
    ,module GUI.BaseLayer.Depend0.Types
    ,module GUI.BaseLayer.Depend0.Auxiliaries
    ,module GUI.BaseLayer.Depend0.Cursor
    ,module GUI.BaseLayer.Depend0.Keyboard
    ,module GUI.BaseLayer.Depend0.Ref
    ,module GUI.BaseLayer.Depend1.Color
    ,module GUI.BaseLayer.Depend1.Geometry
    ,module GUI.BaseLayer.Depend1.Skin
    ,module GUI.BaseLayer.Depend1.Logging
    ,module GUI.BaseLayer.Types
    ,module GUI.BaseLayer.Widget
    ,module GUI.BaseLayer.Window
    ,module GUI.BaseLayer.Pipe
    ,module GUI.BaseLayer.Resource
    ,module GUI.BaseLayer.Canvas
    ,module GUI.BaseLayer.Core
    ,module GUI.BaseLayer.RunGUI
    ,module GUI.BaseLayer.Action
    ,module GUI.BaseLayer.UniqueCode
    ,module GUI.BaseLayer.GUIRecord
    ,module GUI.BaseLayer.SpecStateWidget
    ,module GUI.BaseLayer.Focus
    ,module GUI.BaseLayer.Mouse
    ,module GUI.Widget.Types
  ) where

import GUI.BaseLayer.Depend0.BitFlags
import GUI.BaseLayer.Depend0.Types (
        Coord,GuiSize,GuiCoordOffset,GuiPoint,GuiRect
        ,ColorComponent,GuiTransparency,GuiColor,GuiCurDrawColor
        ,MarginLTRB(..),GuiMargin,WidgetMargin(..),GuiWindowId,GuiWindowIx)
import GUI.BaseLayer.Depend0.Auxiliaries (
        getAppName,withUTF8,withUtf8Locale
        ,showErrMsgBoxT,showErrMsgBoxTL,showErrMsgBoxB,showErrMsgBox)
import GUI.BaseLayer.Depend0.Cursor (CursorIx(..))
import GUI.BaseLayer.Depend0.Keyboard
import GUI.BaseLayer.Depend0.Ref
import GUI.BaseLayer.Depend1.Color
import GUI.BaseLayer.Depend1.Geometry
import GUI.BaseLayer.Depend1.Skin
import GUI.BaseLayer.Depend1.Logging (LogDateTime(..),GUILogDef(..))
import GUI.BaseLayer.Types (WidgetFlags,WindowFlags,Widget,Window,Gui,GuiNotifyCode(..)
        ,WidgetFunctions(..),GuiWidgetCollection,WindowRetcode(..)
        ,WidgetRecord,WindowRecord,GUIRecord)
import GUI.BaseLayer.Widget (
         -- * Виджет Базового уровня.
         -- ** Низкоуровневые функции доступа к полям записи виджета.
         getWidgetParent,getWidgetRect,setWidgetRect
        ,getWidgetCanvasRect,setWidgetCanvasRect,getWidgetFns,setWidgetFns
        ,getWidgetCursorIx,setWidgetCursorIx,getWidgetWindow
         -- ** Флаги виджета
        ,pattern WidgetNoFlags, pattern WidgetRedrawFlag, pattern WidgetSelectable, pattern WidgetEnable
        ,pattern WidgetVisible ,pattern WidgetFocusable,pattern WidgetTabbed, pattern WidgetMouseWheelControl
        ,pattern WidgetFocused
        ,getWidgetFlags,widgetFlagsAdd,widgetFlagsRemove,widgetFlagsAddRemove,allWidgetFlags
        ,anyWidgetFlags,isWidgetMarkedForRedrawing
         -- ** Поля (margin) виджета и функции использующие их.
        ,getWidgetMargin,setWidgetMargin,setWidgetMargin',getWidgetMarginSize,getWidgetRectWithMargin
        ,setWidgetRectWithMarginShrink,calcWidgetSizeWithMargin,widgetResizingIfChanged
        ,getWidgetVisibleRect,getVisibleRect
         -- ** Функции использующие вектор дочерих виджетов.
        ,getWidgetChildrenCount,getChildWidgetIx,getWidgetChild,swapChildWidgets
         -- *** Map-ы по вектору виджетов.
        ,mapByWidgetChildren,imapByWidgetChildren,mapByWidgetChildren_,imapByWidgetChildren_
         -- *** Свёртки по вектору виджетов.
        ,foldByWidgetChildren,foldByWidgetChildren',ifoldByWidgetChildren,ifoldByWidgetChildren'
        ,foldByWidgetChildren_,foldByWidgetChildren'_,ifoldByWidgetChildren_,ifoldByWidgetChildren'_
        -- ** Функции извлечения информации из родительского виджета.
        ,getWidgetParentIx,getWidgetParentFns,getPrevNextWidgets
        -- ** Оперции с деревом виджетов.
        ,forEachWidgets,isMainWidget,getWinMainWidget
        ,findWidgetInTreeForward,findWidgetInTreeBackward,findNextTabbedWidget,findPrevTabbedWidget
        -- ** Доступ к данным 'Window' или 'Gui' через виджет.
        ,getGuiFromWidget,getSkinFromWidget
        -- ** Использование виджета базового уровня
        ,markWidgetForRedraw
        -- *** Изменение координат виджета.
        ,moveWidget,resizeWidget,resizeWidgetWithCanvas,getWidgetCoordOffset
        ,notifyParentAboutSize,simpleOnResizing,extendableOnResizing
        -- *** Поиск и преобразование из клиентских координат в координаты виджета.
        ,coordToWidget,mouseToWidget
        -- ** Логирование и обработка прерываний в контексте виджета.
        ,logPutLnWidget,logOnErrInWidget
        -- ** Отладочные функции вывода парметров виджета(ов) в виде строки.
        ,widgetCoordsToStr,showWidgets,showWidgetsFromMain,showWinWidgets
        -- * Виджет пользовательского уровня.
        ,GuiWidget(..),setWidgetFlag,enableWidget,visibleWidget,fnsCorrectionForTransparent
        -- ** Простейший виджет пользовательского уровня не имеюший своих параметров.
        ,SimpleWidget(..),mkSimpleWidget
        --  * Создание виджетов.
        ,mkWidget',mkWidget,createWidget
             )
import GUI.BaseLayer.Window (
        -- * Низкоуровневые функции доступа к полям записи окна.
         getSDLWindow,getWindowRenderer,getWindowGui,getWinIx,getWindowMainWidget
        ,getWindowForegroundWidget,getFocusedWidget,setFocusedWidget
        ,getWinCursorIx,getWinMainMenu,setWinMainMenu,getWinRetcode,setWinRetcode,setWinOnClosed
        ,setWinOnCloseConfirm
        -- * Обращение к оконным функциям SDL
        ,getWindowAbsolutePosition,fromWinToScreenPoint,getFocusedWin
        -- * Флаги окна.
        ,pattern WindowNoFlags,pattern WindowRedrawFlag,pattern WindowCloseOnLostFocuse,pattern WindowWaitAlt
        ,pattern WindowPopupFlag,pattern WindowLocked,pattern WindowHaveKeyboardFocus,pattern WindowHaveMouseFocus
        ,pattern WindowClickable,pattern WindowCloseOnEsc
        ,getWindowFlags,windowFlagsAddRemove,windowFlagsAdd
        ,windowFlagsRemove,allWindowFlags,anyWindowFlags
        -- * Логирование и обработка ошибок.
        ,logPutLnWindow,logOnErrInWindow',logOnErrInWindow
        -- * Прочее.
        ,getSkinFromWin )
import GUI.BaseLayer.Pipe (
        GuiPipeId,GuiPipeProducer,GuiPipe(newGuiPipe,replaceGuiPipeHandler,sendToGuiPipe)
        ,getPipeIdFromProducer,delGuiPipe )
import GUI.BaseLayer.Resource (ResourceManager,GuiFontOptions(..),GuiFontDef(..)
        ,rmGetFont,rmLoadFont,rmGetCursor,rmAddCursor,rmGetB)
import GUI.BaseLayer.Canvas
import GUI.BaseLayer.Core (
        -- * Создание окон.
        newWindow',newWindow,newModalWindow',newModalWindow
        -- * Композиция виджетов. Присоединение виджета к контейнеру для составления дерева виджетов.
        ,WidgetComposer(..),newForeground
        -- * Действия захватывающие все окна.
        ,forEachWidgetsInAllWin,notifyEachWidgetsInAllWin,setGuiState,getGuiState
        -- * Удаление окон.
        ,delWindowByIx,delWindow,delWindowsBy,delAllPopupWindows
        -- * Удаление виджетов.
        ,delWidget,delAllChildWidgets
        -- * Перестановки дочерних виджетов.
        ,lastChildReplaceFirst
        -- * Наборы функций - обработчиков событий базового виджета.
        ,overlapsChildrenFns
        -- * Изменение окна.
        ,setWinSize',setWinSize,setWinRect,setWinNearer
        -- * Proxy Canvas.
        ,runProxyWinCanvas,runProxyCanvas
                                           )
import GUI.BaseLayer.RunGUI
import GUI.BaseLayer.Action (
    -- GUI.BaseLayer.Action.Internal.Action
     ActionMask,GuiState(..),ActionValue(..),Action(..),Actions
    ,ActionState(..),ActionFn(..)
    ,pattern AllInvisibleActionMask,pattern AllDisableActionMask,pattern AllEnableActionMask
    ,fromActionMask,getActionValueState,isVisibleActionState
    ,mkActionMask
    -- GUI.BaseLayer.Action
    ,addActions,getActionByGroupAndId,getActionsByGroupAndId,setActionEnable
    ,setAction,getVisibleActions

    -- * Функции, упрощающие назначение горячих клавиш при описании действий в пользовательском коде.
    ,kNoModifier,kCtrl,kShift,kAlt,kCtrlShift,kCtrlAlt,kShiftAlt
    )
import GUI.BaseLayer.UniqueCode
import GUI.BaseLayer.GUIRecord (
    -- * Функции, просто возвращающие значения полей.
    guiGetSkin,getWindowsMap,guiGetResourceManager
    -- * Функции подстановки натурального языка.
    ,getB,getT
    -- * Функции использующие вектор окон @guiWindows :: GuiWindowCollection@ .
    ,getWindowsCount,getWindowByIx,doForWinByIx,allWindowsMap_
    ,windowsFold
    -- ** Отладочные.
    ,showWindowsAsStr
    -- * Логирования и обработки исключений.
    ,logPutLn,guiOnSomeException,logOnErr
    -- * Завершение выполнения GUI приложения.
    ,guiApplicationExitSuccess,guiApplicationExitFailure,guiApplicationExitWithCode
                                   )
import GUI.BaseLayer.SpecStateWidget (
    setMouseCapturedWidget,getMouseCapturedWidget,resetMouseCaptured,resetMouseCapturedWidget)
import GUI.BaseLayer.Focus (clearFocusInWindow,clearWidgetFocus,clearFocus
                           ,setWidgetFocus,setFocus)
import GUI.BaseLayer.Mouse
import GUI.Widget.Types
