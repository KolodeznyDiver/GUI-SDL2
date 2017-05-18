{-# LANGUAGE RankNTypes #-}
module GUI.BaseLayer.GUI(
    runGUI
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Int
import Data.Bits
import qualified SDL
import qualified SDL.Raw as Raw
import SDL.Vect
import qualified SDL.TTF
import qualified SDL.Image as IMAGE
import GUI.BaseLayer.Types
import GUI.BaseLayer.Ref
--import Control.Monad.IO.Class (MonadIO)
import GUI.BaseLayer.Internal.Types
import GUI.BaseLayer.Widget
import GUI.BaseLayer.Window
import GUI.BaseLayer.Event
import GUI.BaseLayer.Resource
import qualified GUI.BaseLayer.Primitives as P
import GUI.BaseLayer.Skin
import GUI.BaseLayer.Cursor
import GUI.BaseLayer.Utils
import qualified Data.Map.Strict as Map
import Control.Exception
import GHC.IO.Encoding
import qualified Data.Text as T
import Control.Monad
import Maybes (whenIsJust)
import MonadUtils (whenM)

runGUI:: Skin -> [GuiFontDef] -> (Gui -> IO ()) -> IO ()
runGUI skin fntLst initFn = do
  setForeignEncoding utf8
  bracket_ (SDL.initialize [ SDL.InitVideo, SDL.InitEvents {-, SDL.InitTimer -} ]) SDL.quit $
    SDL.TTF.withInit $
      bracket_ (IMAGE.initialize [IMAGE.InitJPG,IMAGE.InitPNG]) IMAGE.quit $
        bracket (initResourceManager (skinName skin) fntLst) destroyResourceManager $ \rm -> do
            usrEvCode <- Raw.registerEvents 1
            gui <- newMonadIORef GUIStruct    { guiWindows = Map.empty
                                                , guiSkin = skin
                                                , userEventCodeBase = usrEvCode
                                                , resourceManager = rm
                                                }
            let freeFinally = delAllWindows gui
            (`finally` freeFinally) $ do
                initFn gui
                redrawAll gui
                let mainLoop = do
                        (SDL.Event _ evpl) <- SDL.waitEvent
                        unless (evpl == SDL.QuitEvent)
                            (onEvent gui evpl >> mainLoop)
                mainLoop

onEvent:: Gui -> SDL.EventPayload -> IO ()
onEvent gui evpl = case evpl of
    -- A window has been shown.
    SDL.WindowShownEvent (SDL.WindowShownEventData win) -> do
        putStrLn "WindowShownEventData"
        return $ const () win
     -- A window has been hidden.
    SDL.WindowHiddenEvent (SDL.WindowHiddenEventData win) -> do
        putStrLn "WindowHiddenEvent"
        return $ const () win
    -- A part of a window has been exposed - where exposure means to become visible
    -- (for example, an overlapping window no longer overlaps with the window).
    SDL.WindowExposedEvent (SDL.WindowExposedEventData win) -> do
        putStrLn "WindowExposedEvent"
        withWindow win $ \rfWin -> redrawWindow rfWin True
    -- A Window has been moved.
    SDL.WindowMovedEvent (SDL.WindowMovedEventData win newPosPoint) -> return $ const () (win,newPosPoint)
    -- Window has been resized. This is event is always preceded by WindowSizeChangedEvent.
    SDL.WindowResizedEvent (SDL.WindowResizedEventData win newSz)     -> -- do
--        putStrLn $ concat ["WindowResizedEvent  newSz=",show newSz]
        withWindow win $ \rfWin -> do
            widget <- getWindowMainWidget rfWin
            oldRect <- getWidgetRect widget
            let r = SDL.Rectangle zero (fmap fromIntegral newSz)
            when (r /= oldRect) $ do
                fs <- getWidgetFns widget
                onResizing fs widget r
                redrawWindow rfWin True
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
        putStrLn "WindowRestoredEvent"
        return $ const () (win)
    --
    SDL.WindowGainedMouseFocusEvent (SDL.WindowGainedMouseFocusEventData win) ->
        (putStrLn "WindowGainedMouseFocusEvent") >> (return $ const () (win))
    --
    SDL.WindowLostMouseFocusEvent (SDL.WindowLostMouseFocusEventData win) ->
        withWindow win $ \rfWin -> do
            putStrLn "WindowLostMouseFocusEvent" -- >> (return $ const () (win))
            winMouseLost rfWin
    --
    SDL.WindowGainedKeyboardFocusEvent (SDL.WindowGainedKeyboardFocusEventData win) ->
        (putStrLn "WindowGainedKeyboardFocusEvent") >> (return $ const () (win))
    --
    SDL.WindowLostKeyboardFocusEvent (SDL.WindowLostKeyboardFocusEventData win) ->
        (putStrLn "WindowLostKeyboardFocusEvent") >> (return $ const () (win))
    --
    SDL.WindowClosedEvent (SDL.WindowClosedEventData win) -> do
        delWindowByIx gui win
        putStrLn "WindowClosedEvent"
    -- A keyboard key has been pressed or released.
    SDL.KeyboardEvent (SDL.KeyboardEventData
                win
                motion -- SDL.Released | SDL.Pressed  --  data InputMotion = Released | Pressed
                repeated -- True if this is a repeating key press from the user holding the key down.
                _y@(SDL.Keysym
                    _ -- (SDL.Scancode  _scancode) -- Word32
                    keycode -- (SDL.Keycode keycode) -- Int32
                    km -- SDL.KeyModifier
                  )
                      ) ->  -- do
--        putStrLn $ concat [ "KeyboardEvent  motion=",show motion,"  repeated=",show repeated," keysym=",show y]
        withWindow win $ \rfWin -> do
            let (shifted,ctrled,alted) = getShftCtrlAlt km
            waitAlt <- allWindowFlags rfWin WindowWaitAlt
            if isAltKey keycode && not shifted && not ctrled then
                if motion == SDL.Pressed then
                    unless waitAlt $ windowFlagsAdd rfWin WindowWaitAlt
                else when waitAlt $ do
                        windowFlagsRemove rfWin WindowWaitAlt
                        putStrLn "GUI KeyboardEvent : main menu call"
                        mmMb <- getWinMainMenu rfWin
                        whenIsJust mmMb $ \widget -> do
                            fns <- getWidgetFns widget
                            onMouseButton fns widget SDL.Pressed SDL.ButtonLeft 1 zero
            else when waitAlt $ windowFlagsRemove rfWin WindowWaitAlt
            mbf <- getFocusedWidget rfWin
            whenIsJust mbf $ \widget -> do
                let onOrdinalKey = do   fs <- getWidgetFns widget
--                                        putStrLn $ concat [ "KeyboardEvent,onOrdinalKey  motion=",
--                                            show motion,"  keycode=",show keycode]
                                        onKeyboard fs widget motion repeated keycode km
                if motion == SDL.Pressed then do
                    fl <- getWidgetFlags widget
                    case (keycode,shifted,ctrled,alted) of
                        (SDL.KeycodeTab,shiftTab,False,False) | (fl .&. WidgetTabbed) /= WidgetNoFlags -> do
                            n <- (if shiftTab then findPrevTabbedWidget else findNextTabbedWidget) widget
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
    SDL.TextInputEvent (SDL.TextInputEventData win text) -> do
        putStrLn $ concat ["TextInputEvent text=",T.unpack text]
        return $ const () (win)
    -- A mouse or pointer device was moved.
    SDL.MouseMotionEvent (SDL.MouseMotionEventData
                win _
                btnsLst -- [MouseButton] --  data InputMotion = Released | Pressed
                posPointer -- The new position of the mouse. (Point V2 Int32)
                relMv -- The relative mouse motion of the mouse. (V2 Int32)
                         ) -> -- do
--            putStrLn $ concat [ "MouseMotionEvent  relMv=",show relMv]
        let mouseMotionHandler fs widget pnt = onMouseMotion fs widget btnsLst pnt
                                                       (fromIntegral <$> relMv) in
        onMouseAction win posPointer
            -- Main event handler
            mouseMotionHandler
            -- Mouse captured handler
            mouseMotionHandler
    -- A mouse button was pressed or released.
    SDL.MouseButtonEvent (SDL.MouseButtonEventData
                win motion _
                mouseButton -- MouseButton = ButtonLeft | ButtonMiddle | ButtonRight | ButtonX1 | ButtonX2 | ButtonExtra Int
                clicks -- The amount of clicks. 1 for a single-click, 2 for a double-click, etc.
                posPointer -- (Point V2 Int32)
                         ) ->  -- do
{-            putStrLn $ concat [ "MouseButtonEvent  motion=",show motion
               ,"  mouseButton=",show mouseButton
               ," clicks=",show clicks,"  posPointer=",show posPointer ] -}
        let mouseButtonHandler fs widget pnt = onMouseButton fs widget motion mouseButton (fromIntegral clicks) pnt in
        onMouseAction win posPointer
            -- Main event handler
            mouseButtonHandler
            -- Mouse captured handler
            mouseButtonHandler
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
                        if found then do    fs <- getWidgetFns widg
--                                            putStrLn "MouseWheelEvent : WheelControl found"
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
    SDL.UserEvent (SDL.UserEventData win code _ _) ->
        case toEnum $ fromIntegral code of
            RedrawRequestWidget -> redrawWindowByIx gui win False
            RedrawRequestWindow -> redrawWindowByIx gui win True
            RedrawRequestAll    -> redrawAll gui
    _ ->  return ()

  where isAltKey k = SDL.KeycodeLAlt == k ||  SDL.KeycodeRAlt == k -- ||  SDL.KeycodeAltEras == k
        isWidgetEnable widget = allWidgetFlags widget $ WidgetVisible .|. WidgetEnable
        widgetMouseLostNotify Nothing    = return ()
        widgetMouseLostNotify (Just widget) = whenM (isWidgetEnable widget) $
            do {fs <- getWidgetFns widget; onLostMouseFocus fs widget}
--        withWindow :: MonadIO m => GuiWindowIx -> (GuiWindow -> m ()) -> m ()
        withWindow win f = getWindowByIx gui win >>= (`whenIsJust` f) -- (\w -> f w >> redrawAll gui))
        winMouseLost rfWin = do
                                        ow <- getWidgetUnderCursor rfWin
                                        widgetMouseLostNotify ow
                                        setWidgetUnderCursor rfWin Nothing
                                        setWinCursorIx rfWin DefCursorIx
                                        resetMouseCaptured rfWin
                                        redrawAll gui
        onMouseAction ::  forall m. MonadIO m => SDL.Window -> Point V2 Int32 ->
                            -- Main event handler
                            (WidgetFunctions -> Widget -> GuiPoint -> m ()) ->
                            -- Mouse captured handler
                            (WidgetFunctions -> Widget -> GuiPoint -> m ()) -> m ()
        onMouseAction win posPointer action capturedAction =
            withWindow win $ \ rfWin -> do
                let pnt = P.mousePointToGuiPoint posPointer
                mbCapt <- getMouseCapturedWidget rfWin
                case mbCapt of
                    Just widget -> do
                         fs <- getWidgetFns widget
                         offset <- getWidgetCoordOffset widget
                         capturedAction fs widget $  pnt .-^ offset
                         redrawAll gui
                    _ -> do mbWO <- mouseToWidget rfWin pnt
                            ow <- getWidgetUnderCursor rfWin
                            let nw = fst <$> mbWO
                            when (nw /= ow) $ do
                                widgetMouseLostNotify ow
                                whenIsJust mbWO $ \(widget,offset) ->
                                    whenM (isWidgetEnable widget) $ do
                                        fs <- getWidgetFns widget
                                        onGainedMouseFocus fs widget $ pnt .-^ offset
                                setWidgetUnderCursor rfWin nw
                            nc <- case mbWO of
                                        Just (widget,offset) ->  do
                                                ena <- isWidgetEnable widget
                                                if ena then do
                                                    fs <- getWidgetFns widget
                                                    action fs widget $ pnt .-^ offset
                                                    getWidgetCursorIx widget
                                                else return DefCursorIx
                                        _ -> return DefCursorIx
                            oc <- getWinCursorIx rfWin
                            when (nc /= oc) $ do
                                guiSetCursor gui nc
                                setWinCursorIx rfWin nc
                            redrawAll gui


