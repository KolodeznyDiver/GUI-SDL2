{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}
module GUI.Widget.Handlers(
    colorRectFns,grayRectFns
    ,MouseAnimatedHndlr(..),MouseAnimatedClickableHndlr(..)
    ,noChildrenClickableHndlr,noChildrenMouseAnimatedHndlr
        ) where

import Control.Monad.IO.Class -- (MonadIO)
import Control.Monad
import Data.IORef
import qualified SDL
--import SDL.Vect
import GUI
--import qualified Data.Vector as V
--import Data.Default

colorRectFns :: GuiSize -> GuiColor -> WidgetFunctions
colorRectFns sz color = (noChildrenFns sz){
    onDraw= \widget -> setColor color >> getVisibleRect widget >>= fillRect
                                           }

grayRectFns:: GuiSize -> ColorComponent -> WidgetFunctions
grayRectFns sz = colorRectFns sz . grayColor
{-# INLINE grayRectFns #-}

data MouseAnimatedHndlr = MouseAnimatedHndlr
        { mouseAnimatedMouseState :: IORef WidgetMouseState
        , mouseAnimatedOnClick :: forall m. MonadIO m => Widget -> Bool -> GuiPoint -> m ()
        , mouseAnimatedFs :: WidgetFunctions
        }

noChildrenMouseAnimatedHndlr :: forall m. MonadIO m => GuiSize ->
        (forall n. MonadIO n => Widget -> Bool -> GuiPoint -> n ()) -> m MouseAnimatedHndlr
noChildrenMouseAnimatedHndlr sz onClickAction = do
    mouseState <- newMonadIORef WidgetMouseOut
    let clickHandler widget pressed pnt = do
                writeMonadIORef mouseState $ if | pressed -> WidgetMousePressed
                                                | pnt == KbdClickSpecPoint -> WidgetMouseOut
                                                | otherwise -> WidgetMouseIn
                markWidgetForRedraw widget
                onClickAction widget pressed pnt
    return (MouseAnimatedHndlr mouseState clickHandler (noChildrenFns sz){
        onGainedMouseFocus = \widget _ {--pnt-} ->
            writeMonadIORef mouseState WidgetMouseIn >> markWidgetForRedraw widget
        ,onLostMouseFocus = \widget -> writeMonadIORef mouseState WidgetMouseOut >> markWidgetForRedraw widget
        ,onMouseButton = \widget motion mouseButton _ {-clicks -} pnt -> do
            ena <- allWidgetFlags widget WidgetEnable
            when (ena && (mouseButton == SDL.ButtonLeft)) $ clickHandler widget (motion==SDL.Pressed) pnt
                               })

data MouseAnimatedClickableHndlr = MouseAnimatedClickableHndlr
        { mouseAnimatedClickableMouseState :: IORef WidgetMouseState
        , mouseAnimatedClickableAction :: IORef NoArgAction
        , mouseAnimatedClickableFs :: WidgetFunctions
        }

noChildrenClickableHndlr :: forall m. MonadIO m => GuiSize ->
        (forall n. MonadIO n => Widget -> Bool -> GuiPoint -> n ()) ->
        m MouseAnimatedClickableHndlr
noChildrenClickableHndlr sz onClickAction = do
    onCLick' <- newMonadIORef $ NoArgAction $ return ()
    let onClickAction' w b p = when b (join $ noArgAction <$> readMonadIORef onCLick') >>
                                                   onClickAction w b p
    MouseAnimatedHndlr{ mouseAnimatedMouseState = mouseState
                       , mouseAnimatedOnClick = clickHandler
                       , mouseAnimatedFs = fns }
                 <- noChildrenMouseAnimatedHndlr sz onClickAction'
    return (MouseAnimatedClickableHndlr mouseState onCLick' fns{
        onKeyboard = \widget motion _repeated keycode km ->
                        let (shifted,ctrled,alted) = getShftCtrlAlt km in
                        when ((shifted,ctrled,alted) == (False,False,False) &&
                           (isEnterKey keycode || keycode== SDL.KeycodeSpace)) $ -- do
{-                                sDbg <- widgetCoordsToStr widget
                                ms <- readMonadIORef mouseState
                                liftIO $ putStrLn $ concat ["ClickableFns.onKeyboard ",
                                    show (motion==SDL.Pressed), "    ", sDbg, "   ", show ms] -}
                                clickHandler widget (motion==SDL.Pressed) KbdClickSpecPoint
{-                                ms2 <- readMonadIORef mouseState
                                liftIO $ putStrLn $ concat ["ClickableFns.onKeyboard after ",
                                     show ms2] -}
                                    })


