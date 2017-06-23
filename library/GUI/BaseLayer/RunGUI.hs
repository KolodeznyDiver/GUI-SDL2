{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:      GUI.BaseLayer.RunGUI
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <kolodeznydiver@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Модуль содержащий основную функцию @runGUI@ с которой начинается выполнения GUI.

module GUI.BaseLayer.RunGUI(
    -- * Основная функция GUI и её параметры.
    GUIDef(..),runGUI
  ) where

import Foreign
import Control.Monad.IO.Class -- (MonadIO)
import qualified Data.Map.Strict as Map (empty)
import Control.Exception
import Control.Monad
import System.Directory
import Data.Default
import Data.Maybe
import Maybes (whenIsJust)
import MonadUtils (whenM)
import qualified SDL
import qualified SDL.Raw as Raw
import qualified SDL.Internal.Types
import SDL.Vect
import qualified SDL.TTF
import qualified SDL.Image as IMAGE
import GUI.BaseLayer.Depend0.Types
import GUI.BaseLayer.Depend0.Auxiliaries
import GUI.BaseLayer.Depend0.Ref
import GUI.BaseLayer.Depend0.Cursor
import GUI.BaseLayer.Depend0.Keyboard
import GUI.BaseLayer.Depend1.Skin
import GUI.BaseLayer.Depend1.Logging hiding (logPutLn,logOnErr)
import GUI.BaseLayer.Types
import GUI.BaseLayer.Widget
import GUI.BaseLayer.Pipe
import GUI.BaseLayer.Resource
import qualified GUI.BaseLayer.Primitives as P
import GUI.BaseLayer.Core
import GUI.BaseLayer.Action
import System.Utils (hideConsole)
import GUI.BaseLayer.Window
import GUI.BaseLayer.RedrawWindow
import GUI.BaseLayer.SpecStateWidget
import GUI.BaseLayer.GUIRecord
import GUI.BaseLayer.Focus

-- | Необязательные параметры инициализации GUI.
data GUIDef = GUIDef {
      -- | Параметры журналирования
      guiLogDef :: GUILogDef
      -- | Оставлять ли консольное окно видимым (и выводить дублировать ли в него сообщения журанал).
    , guiConsoleVisible :: Bool
      -- | Явное указание директории данных для программы.
      -- В этой директории по умолчанию создаётся журнал.
      -- Так же, в её подкаталоге храняться ресурсы приложения, см. "GUI.BaseLayer.Resource".
      -- Если строка пуста, вместо неё используется результат выполнения
      -- @getXdgDirectory XdgData \"ИмяПриложения\"@ (см. документацию к пакету __directory__).
    , guiDataDirectory :: String
                     }

instance Default GUIDef where
    def = GUIDef { guiLogDef = def
                 , guiConsoleVisible = True
                 , guiDataDirectory = "" -- set default. Not current dur.
                 }

-- | Функция с которой начинается выполнения GUI.
runGUI:: Skin -> -- ^ Текущий 'Skin' описывающий оформление интерфейса.
        [GuiFontDef] -> -- ^ Таблица шрифтов.
        GUIDef ->  -- ^ Необязательные параметры GUI.
        (Gui -> IO ()) ->  -- ^ Функция конструирующая (нальную часть) GUI.
        IO ()
runGUI skin fntLst GUIDef{..} initFn =
  bracket_ (SDL.initialize [ SDL.InitVideo, SDL.InitEvents {-, SDL.InitTimer -} ]) SDL.quit $ do
    unless guiConsoleVisible
        hideConsole -- При желании спрячем консольное окно (только Windows, для X11 пока ничего не делает)
    dataDir <- if null guiDataDirectory then
                    -- for ex. C:\Users\User\AppData\Roaming\GUIDemo
                    --      or ~/.local/share/GUIDemo
                    getXdgDirectory XdgData =<< getAppName
               else return guiDataDirectory
    mbLog <- guiLogStart guiLogDef dataDir guiConsoleVisible
    whenIsJust mbLog $ \gLog -> SDL.TTF.withInit $
        bracket_ (IMAGE.initialize [IMAGE.InitJPG,IMAGE.InitPNG]) IMAGE.quit $
        bracket (initResourceManager (skinName skin) fntLst dataDir gLog) destroyResourceManager $ \rm -> do
            usrEvCode <- Raw.registerEvents 1
            gui <- newMonadIORef GUIRecord    { guiWindows = Map.empty
                                              , guiSkin = skin
                                              , userEventCodeBase = usrEvCode
                                              , resourceManager = rm
                                              , guiActions = mkEmptyActions
                                              , guiState = GuiState 0
                                              , guiUnique = 0
                                              , guiPipes = mkGuiPipes
                                              , guiLog = gLog
                                              , guiModalWins = []
                                              }
            let freeFinally = do
                    delAllWindows gui
--                    putStrLn "freeFinally"
                    guiGetLog gui >>= guiLogStop
                mainLoop = do
                    (SDL.Event _ evpl) <- SDL.waitEvent
                    unless (evpl == SDL.QuitEvent)
                        (onEvent gui evpl >> mainLoop)
            (`finally` freeFinally) $ (`guiCatch` guiOnSomeException gui "runGUI") $ do
                initFn gui
                redrawAll gui
                mainLoop

-- | Не экспортируемая функция, вызываемяа только из @runGUI@ для обработки одного SDL сообщения из очереди.
onEvent:: Gui -> SDL.EventPayload -> IO ()
onEvent gui evpl = case evpl of
    -- A window has been shown.
    SDL.WindowShownEvent (SDL.WindowShownEventData win) -> do
--        putStrLn "WindowShownEventData"
        return $ const () win
     -- A window has been hidden.
    SDL.WindowHiddenEvent (SDL.WindowHiddenEventData win) -> do
--        putStrLn "WindowHiddenEvent"
        return $ const () win
    -- A part of a window has been exposed - where exposure means to become visible
    -- (for example, an overlapping window no longer overlaps with the window).
    SDL.WindowExposedEvent (SDL.WindowExposedEventData win) -> do
--        putStrLn "WindowExposedEvent"
        withWindow win $ \rfWin -> redrawWindow rfWin True
    -- A Window has been moved.
    SDL.WindowMovedEvent (SDL.WindowMovedEventData win newPosPoint) -> return $ const () (win,newPosPoint)
    -- Window has been resized. This is event is always preceded by WindowSizeChangedEvent.
    SDL.WindowResizedEvent (SDL.WindowResizedEventData win newSz)     -> -- do
--        putStrLn $ concat ["WindowResizedEvent  newSz=",show newSz]
        withWindow win $ \rfWin -> do
            setWinSize' rfWin $ fmap fromIntegral newSz
            redrawWindow rfWin False
--        return $ const () (win, newSz)
    -- The window size has changed, either as a result of an API call or through the system or user changing the window size;
    -- this event is followed by WindowResizedEvent if the size was changed by an external event,
    -- i.e. the user or the window manager.
    SDL.WindowSizeChangedEvent (SDL.WindowSizeChangedEventData win) -> -- do
--        putStrLn "WindowSizeChangedEvent"
        return $ const () (win)
    --
    SDL.WindowMinimizedEvent (SDL.WindowMinimizedEventData win) ->  return $ const () (win)
    --
    SDL.WindowMaximizedEvent (SDL.WindowMaximizedEventData win) ->  return $ const () (win)
    --
    SDL.WindowRestoredEvent (SDL.WindowRestoredEventData win) -> do
        logPutLn gui "WindowRestoredEvent"
        return $ const () (win)
    --
    SDL.WindowGainedMouseFocusEvent (SDL.WindowGainedMouseFocusEventData win) ->
        withWindow win $ \rfWin -> -- do
            --logPutLn gui "WindowGainedMouseFocusEvent"
            windowFlagsAdd rfWin WindowHaveMouseFocus
    --
    SDL.WindowLostMouseFocusEvent (SDL.WindowLostMouseFocusEventData win) ->
        withWindow win $ \rfWin -> do
--            logPutLn gui "WindowLostMouseFocusEvent"
            windowFlagsRemove rfWin $ WindowHaveMouseFocus .|. WindowClickable
            winMouseLost rfWin

    SDL.WindowGainedKeyboardFocusEvent (SDL.WindowGainedKeyboardFocusEventData win) ->
        withNoLockedWindow win $ \rfWin -> do
            windowFlagsAdd rfWin WindowHaveKeyboardFocus
            fl <- getWindowFlags rfWin
            when ((fl .&. WindowPopupFlag) == WindowNoFlags ) $ delAllPopupWindows gui
{-            (btns,mouseP) <- getMouseState -- SDL.getMouseButtons
            putStrLn $ concat ["WindowGainedKeyboardFocusEvent  btns=", show btns, "   ", show mouseP
                ,"   haveMouseFocus = ", show haveMouseFocus] -}
--            if (fl .&. (WindowHaveMouseFocus .|. WindowClickable)) == WindowHaveMouseFocus then do -- inacive MouseButton message click fix.
            when ((fl .&. (WindowHaveMouseFocus .|. WindowClickable)) == WindowHaveMouseFocus) $ do -- inacive MouseButton message click fix.
                (btns,mouseP) <- getMouseState -- SDL.getMouseButtons
              --  putStrLn $ concat ["WindowGainedKeyboardFocusEvent  btns=", show btns, "   ", show mouseP]
                when (btns ==0) $ do
                    windowFlagsAdd rfWin WindowClickable
                    onMouseButton' win SDL.Pressed SDL.ButtonLeft 1 $ fmap fromIntegral mouseP
            --else logPutLn gui "WindowGainedKeyboardFocusEvent"
    --
    SDL.WindowLostKeyboardFocusEvent (SDL.WindowLostKeyboardFocusEventData win) ->
        withWindow win $ \rfWin -> do
            closeOnLost <- allWindowFlags rfWin WindowCloseOnLostFocuse
            if closeOnLost then
--                putStrLn "WindowLostKeyboardFocusEvent delWindow"
                 delWindow rfWin
            else windowFlagsRemove rfWin WindowHaveKeyboardFocus -- .|. WindowClickable
--            putStrLn "WindowLostKeyboardFocusEvent"
    --
    SDL.WindowClosedEvent (SDL.WindowClosedEventData win) -> -- do
      withNoLockedWindow win $ \_rfWin -> do
--        cWins' <- getWindowsCount gui
        delWindowByIx gui win
--        cWins <- getWindowsCount gui
--        putStrLn $ concat ["WindowClosedEvent cWins before = ", show cWins', "   cWins after = ", show cWins]
{-        when (cWins>0) $ do
            popupOnly <- windowsFold gui (\b rfWin -> (b &&) <$> allWindowFlags rfWin WindowPopupFlag) True
            when popupOnly $ guiApplicationExitSuccess gui -}
        cNoPopupWins <- windowsFold gui (\n rfWin -> do
                            isPopup <- allWindowFlags rfWin WindowPopupFlag
                            return (if isPopup then n else n+1)) (0::Int)
        when (cNoPopupWins==0) $ guiApplicationExitSuccess gui
    -- A keyboard key has been pressed or released.
    SDL.KeyboardEvent (SDL.KeyboardEventData
                win
                motion -- SDL.Released | SDL.Pressed  --  data InputMotion = Released | Pressed
                repeated -- True if this is a repeating key press from the user holding the key down.
                (SDL.Keysym
                    _ -- (SDL.Scancode  _scancode) -- Word32
                    keycode -- (SDL.Keycode keycode) -- Int32
                    km -- SDL.KeyModifier
                  )
                      ) ->
        do let sca@ShiftCtrlAlt{..} = getShftCtrlAlt km
               hkMod = scaToKeyModifier sca
            -- key = SDL.unwrapKeycode keycode
           wasHK <- if motion == SDL.Pressed then chkHotKey gui hkMod keycode else return False
{-           when (motion == SDL.Pressed) $
               putStrLn $ concat [ -- "KeyboardEvent  motion=",show motion, -- "  repeated=",show repeated,
--               " keycode=",show key, "  ",
--               if key<128 then let c= chr $ fromIntegral key in if isPrint c then ['[',c,']'] else ""  else "",
                             "KeyModifiers = ",show hkMod, "   showbKeycode = ", showbKeycode keycode,
                             if wasHK then "   KeyWithModifiers" else ""] -}
           if wasHK then do
                delAllPopupWindows gui
                withWindow win $ \rfWin ->
                                windowFlagsRemove rfWin WindowWaitAlt
                redrawAll gui
           else
                withWindow win $ \rfWin -> do
                    waitAlt <- allWindowFlags rfWin WindowWaitAlt
                    if isAltKey keycode && not isShift && not isCtrl then
                        if motion == SDL.Pressed then
                            unless waitAlt $ windowFlagsAdd rfWin WindowWaitAlt
                        else when waitAlt $ do
                                windowFlagsRemove rfWin WindowWaitAlt
                                delAllPopupWindows gui
--                                putStrLn "GUI KeyboardEvent : main menu call"
                                mmMb <- getWinMainMenu rfWin
                                whenIsJust mmMb $ \widget -> do
                                    fns <- getWidgetFns widget
                                    logOnErr gui
                                      "onEvent.main menu call.onMouseButton" $
                                      onMouseButton fns widget SDL.Pressed SDL.ButtonLeft 1 zero
                    else when waitAlt $ windowFlagsRemove rfWin WindowWaitAlt
                    mbf <- getFocusedWidget rfWin
                    whenIsJust mbf $ \widget -> do
                        let onOrdinalKey = do
                                fs <- getWidgetFns widget
--                                        putStrLn $ concat [ "KeyboardEvent,onOrdinalKey  motion=",
--                                            show motion,"  keycode=",show keycode]
                                logOnErr gui
                                    "onEvent.KeyboardEvent.onKeyboard" $
                                    onKeyboard fs widget motion repeated keycode km
                        if motion == SDL.Pressed then do
                            fl <- getWidgetFlags widget
                            case (keycode,isShift,isCtrl,isAlt) of
                                (SDL.KeycodeTab,_,False,False) | (fl .&. WidgetTabbed) /= WidgetNoFlags -> do
                                    n <- (if isShift then findPrevTabbedWidget else findNextTabbedWidget) widget
                                    when (n /= widget) $ setWidgetFocus n
                                _ -> onOrdinalKey
                        else onOrdinalKey
                        redrawAll gui
        -- return $ const () (win,scancode,keycode)
{-
    -- Keyboard text editing event information.
    SDL.TextEditingEvent (SDL.TextEditingEventData
                win
                editingText -- Text   The editing text.
                startPos -- The location to begin editing from.
                edLength -- The number of characters to edit from the start point.
                         ) -> do
        putStrLn $ concat ["TextEditingEvent editingText=",T.unpack editingText,"  startPos=",show startPos,"  edLength=",show edLength]
        return $ const () (win)
-}
    --
    SDL.TextInputEvent (SDL.TextInputEventData win text) ->
        withWindow win $ \rfWin -> do
--            putStrLn ("TextInputEvent text=" ++ T.unpack text)
            mbf <- getFocusedWidget rfWin
            whenIsJust mbf $ \widget -> do
                fs <- getWidgetFns widget
                logOnErr gui "onEvent.TextInputEvent.onTextInput" $
                    onTextInput fs widget text

    -- A mouse or pointer device was moved.
    SDL.MouseMotionEvent (SDL.MouseMotionEventData
                win _
                btnsLst -- [MouseButton] --  data InputMotion = Released | Pressed
                posPointer -- The new position of the mouse. (Point V2 Int32)
                relMv -- The relative mouse motion of the mouse. (V2 Int32)
                         ) ->
{-        withWindow win $ \rfWin -> do
            isGainedMouseNow <- allWindowFlags rfWin WindowGainedMouseNow
            when isGainedMouseNow $ do
                windowFlagsRemove rfWin WindowGainedMouseNow
                unless (null btnsLst) $
                    putStrLn $ concat [ "MouseMotionEvent, GainedMouseNow  posPointer="
                        ,show posPointer,"   relMv=", show relMv] -}
--            putStrLn $ concat [ "MouseMotionEvent  relMv=",show relMv]
{-        let mouseMotionHandler _win fs widget pnt =
                onMouseMotion fs widget btnsLst pnt (fromIntegral <$> relMv) in
        onMouseAction win posPointer mouseMotionHandler -}
        onMouseAction win posPointer $ \_rfWin fs widget pnt ->
            onMouseMotion fs widget btnsLst pnt (fromIntegral <$> relMv)
    -- A mouse button was pressed or released.
    SDL.MouseButtonEvent (SDL.MouseButtonEventData
                win motion _
                mouseButton -- MouseButton = ButtonLeft | ButtonMiddle | ButtonRight | ButtonX1 | ButtonX2 | ButtonExtra Int
                clicks -- The amount of clicks. 1 for a single-click, 2 for a double-click, etc.
                posPointer -- (Point V2 Int32)
                         ) -> -- do
--        (btns,mouseP) <- getMouseState -- SDL.getMouseButtons
--        putStrLn $ concat ["MouseButtonEvent  btns=", show btns, "   ", show mouseP]
--        when (motion==SDL.Pressed) $ do
--            putStrLn "MouseButtonEvent Pressed"
{-            putStrLn $ concat [ "MouseButtonEvent  motion=",show motion
               ,"  mouseButton=",show mouseButton
               ," clicks=",show clicks,"  posPointer=",show posPointer ] -}
        onMouseButton' win motion mouseButton clicks posPointer
    -- Mouse wheel event information.
    SDL.MouseWheelEvent (SDL.MouseWheelEventData win _
                pos -- (V2 Int32)                  The amount scrolled.
                dir -- The scroll direction mode. ScrollNormal | ScrollFlipped
                         ) ->  -- do
--        putStrLn $ concat ["MouseWheelEvent  ", show pos, "    ", show dir]
        withWindow win $ \rfWin -> do
            mbf <- getFocusedWidget rfWin
            whenIsJust mbf $ \widget -> do
--                putStrLn "MouseWheelEvent : FocusedWidget found"
                let findWellControll widg = do
                        found <- allWidgetFlags widg $ WidgetMouseWheelControl .|. WidgetEnable
                        if found then do
                            fs <- getWidgetFns widg
--                              putStrLn "MouseWheelEvent : WheelControl found"
                            logOnErr gui "onEvent.MouseWheelEvent.onMouseWheel" $
                                onMouseWheel fs widg (fromIntegral <$> pos) dir
                            redrawAll gui
                        else do parent <- getWidgetParent widg
--                                putStrLn "findWellControll next"
                                when (parent /= widg) $ findWellControll parent
                findWellControll widget
--                putStrLn "findWellControll Exit"
    -- An event used to request a file open by the system
    SDL.DropEvent (SDL.DropEventData cString) ->  putStr "DropEvent  string=" >> print cString
    --
    SDL.ClipboardUpdateEvent ->  return () -- no param
    SDL.UserEvent (SDL.UserEventData win code d1 d2) -> do
        let (SDL.Internal.Types.Window w) = win
        when (w==nullPtr) $ userMsgHandler gui code d1 d2
{-            p1 = ptrToWordPtr d1
            p2 = ptrToWordPtr d2
        putStrLn $ concat ["UserEvent win=", show w, "  code=", show code,
            "   d1=", show $ fromEnum p1, "   d2=", show $ fromEnum p2, " ptr sz= ", show $ finiteBitSize p1 ] -}
    _ ->  return ()

  where isAltKey k = SDL.KeycodeLAlt == k ||  SDL.KeycodeRAlt == k -- ||  SDL.KeycodeAltEras == k
        isWidgetEnable widget = allWidgetFlags widget $ WidgetVisible .|. WidgetEnable
        widgetMouseLostNotify Nothing    = return ()
        widgetMouseLostNotify (Just widget) = whenM (isWidgetEnable widget) $ do
            fs <- getWidgetFns widget
            logOnErr gui "onEvent.widgetMouseLostNotify.onLostMouseFocus" $
                onLostMouseFocus fs widget
--        withWindow :: MonadIO m => GuiWindowIx -> (Window -> m ()) -> m ()
        withWindow win f = getWindowByIx gui win >>= (`whenIsJust` f)
        withNoLockedWindow win f = withWindow win $ \ rfWin -> do
            isLocked <- allWindowFlags rfWin WindowLocked
            if isLocked then do
                g <- readMonadIORef gui
                whenIsJust (listToMaybe $ guiModalWins g)
                    (`withWindow` (SDL.raiseWindow <=< getSDLWindow))
            else f rfWin
        winMouseLost rfWin = do
                                        ow <- getWidgetUnderCursor rfWin
                                        widgetMouseLostNotify ow
                                        setWidgetUnderCursor rfWin Nothing
                                        setWinCursorIx rfWin DefCursorIx
                                        mbCW <- getMouseCapturedWidget rfWin
                                        whenIsJust mbCW $ \ widget -> do
                                            resetMouseCaptured rfWin
                                            markWidgetForRedraw widget
                                        redrawAll gui
        onMouseAction :: SDL.Window -> Point V2 Int32 ->
                            -- Main event handler
                        (Window -> WidgetFunctions -> Widget -> GuiPoint -> IO ()) ->
                            -- Mouse captured handler
--                            (WidgetFunctions -> Widget -> GuiPoint -> m ()) ->
                            IO ()
        onMouseAction win posPointer action {- capturedAction -} =
            withWindow win $ \ rfWin -> do
                let pnt = P.mousePointToGuiPoint posPointer
                    doAction widget offset = do
                        fs <- getWidgetFns widget
                        logOnErr gui "onEvent.onMouseAction.doAction" $
                            action rfWin fs widget $ pnt .-^ offset
                mbCapt <- getMouseCapturedWidget rfWin
                case mbCapt of
                    Just widget -> do
                         offset <- getWidgetCoordOffset widget
                         doAction widget offset
{-                         fs <- getWidgetFns widget
                         capturedAction rfWin fs widget $  pnt .-^ offset -}
                         redrawAll gui
                    _ -> do mbWO <- mouseToWidget rfWin pnt
                            ow <- getWidgetUnderCursor rfWin
                            let nw = fst <$> mbWO
                            when (nw /= ow) $ do
                                widgetMouseLostNotify ow
                                whenIsJust mbWO $ \(widget,offset) ->
                                    whenM (isWidgetEnable widget) $ do
                                        fs <- getWidgetFns widget
                                        logOnErr gui "onEvent.onMouseAction.onGainedMouseFocus" $
                                            onGainedMouseFocus fs widget $ pnt .-^ offset
                                setWidgetUnderCursor rfWin nw
                            nc <- case mbWO of
                                        Just (widget,offset) ->  do
                                                ena <- isWidgetEnable widget
                                                if ena then do
                                                    doAction widget offset
{-                                                    fs <- getWidgetFns widget
                                                    action rfWin fs widget $ pnt .-^ offset -}
                                                    getWidgetCursorIx widget
                                                else return DefCursorIx
                                        _ -> return DefCursorIx
                            oc <- getWinCursorIx rfWin
                            when (nc /= oc) $ do
                                guiSetCursor gui nc
                                setWinCursorIx rfWin nc
                            redrawAll gui
        onMouseButton'  :: SDL.Window -> SDL.InputMotion -> SDL.MouseButton ->
                                Word8 -> Point V2 Int32 -> IO ()
        onMouseButton' win motion mouseButton clicks posPointer =
            let mouseButtonHandler _win fs widget =
                    onMouseButton fs widget motion mouseButton (fromIntegral clicks) in
            onMouseAction win posPointer mouseButtonHandler

getMouseState :: MonadIO m => m (Int,GuiPoint)
getMouseState = liftIO $
    alloca $ \ pX -> alloca $ \ pY -> do
        btnState <- convert <$> Raw.getMouseState pX pY
        x <- peek pX
        y <- peek pY
        return (btnState,P (V2 (fromIntegral x) (fromIntegral y)))
  where convert = fromIntegral
