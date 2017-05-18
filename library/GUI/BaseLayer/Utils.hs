{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module GUI.BaseLayer.Utils(
    WidgetComposer(..),mulDiv,getWidgetCoordOffset,coordToWidget,mouseToWidget
    ,runProxyWinCanvas,runProxyCanvas,guiGetSkin,getSkinFromWin,getSkinFromWidget,guiGetResourceManager
    ,guiSetCursor,getShftCtrlAlt,isEnterKey,clearFocusInWindow,clearWidgetFocus,setWidgetFocus
    ,redrawAll,createWidget,mkWidget,SimpleWidget(..),mkSimpleWidget
    ,delWidget,delAllChildWidgets,lastChildReplaceFirst
    ,delWindowByIx,delWindow,delAllWindows
                 ) where

import Control.Monad
import Control.Monad.IO.Class -- (MonadIO)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Data.Bits
import Maybes (whenIsJust)
import qualified SDL
import SDL.Vect
import GUI.BaseLayer.Types
import GUI.BaseLayer.Ref
import GUI.BaseLayer.Internal.Types
import GUI.BaseLayer.Widget
import GUI.BaseLayer.Window
import GUI.BaseLayer.Geometry
import qualified GUI.BaseLayer.Primitives as P
import GUI.BaseLayer.Skin
import GUI.BaseLayer.Resource
import GUI.BaseLayer.Cursor
import Data.Vector.Utils
import GUI.BaseLayer.Canvas

infixr 0 $+

class WidgetComposer a where
    ($+) :: MonadIO m => a -> (Widget -> Skin -> m (GuiWidget b)) -> m (GuiWidget b)

instance WidgetComposer GuiWindow where
    rfWin $+ initF = do
             mainW <- getWindowMainWidget rfWin
             delAllChildWidgets mainW
             createWidget mainW initF

mulDiv :: Integral a => a -> a -> a -> a
mulDiv a b c = fromInteger $ toInteger a * toInteger b `div` toInteger c
{-# INLINE mulDiv #-}

getWidgetCoordOffset:: MonadIO m => Widget -> m GuiCoordOffset
getWidgetCoordOffset = go zero
    where go sumOff widget = do
            w <- readMonadIORef widget
            let (SDL.Rectangle widgP _) = widgetRect w
                pInWinCoord = widgP .+^ sumOff
                off = pInWinCoord .-. pointOfRect (widgetCanvasRect w)
                parent = parentWidget w
            if widget == parent then return off else go off parent

coordToWidget:: MonadIO m => Widget -> GuiPoint -> m (Maybe (Widget,GuiCoordOffset))
coordToWidget widget' pnt = do
        r <- getWidgetRect widget'
        go zero r widget'
    where go parentOff clipRect widget = do
            w <- readMonadIORef widget
            let (SDL.Rectangle widgP widgSz) = widgetRect w
                pInWinCoord = widgP .+^ parentOff
                rect = rectIntersection clipRect $ SDL.Rectangle pInWinCoord widgSz
            if ((widgetFlags w .&. WidgetVisible) /= WidgetNoFlags) && isInRect rect pnt then
                let off = pInWinCoord .-. pointOfRect (widgetCanvasRect w)
                    byChilds 0 = return $ Just (widget,off)
                    byChilds i = do
                        let ii = pred i
                        r <- go off rect $ V.unsafeIndex (cildrenWidgets w) ii
                        case r of
                            j@(Just _) -> return j
                            _ -> byChilds ii
                in byChilds $ V.length $ cildrenWidgets w
            else
                return Nothing

mouseToWidget:: MonadIO m => GuiWindow -> GuiPoint -> m (Maybe (Widget,GuiCoordOffset))
mouseToWidget rfWin pnt = (mainWidget <$> readMonadIORef rfWin) >>= (`coordToWidget` pnt)
{-# INLINE mouseToWidget #-}

runProxyWinCanvas :: MonadIO m => GuiWindow -> GuiCanvas m a -> m a
runProxyWinCanvas rfWin f = do
    win <- readMonadIORef rfWin
    rm  <- guiGetResourceManager $ guiOfWindow win
    P.withRendererTarget (winRenderer win) (winProxyTexture win) $
        runCanvas (winRenderer win) rm zero f

runProxyCanvas :: MonadIO m => Widget -> GuiCanvas m a -> m a
runProxyCanvas widget f = getWidgetWindow widget >>= (`runProxyWinCanvas` f)
{-# INLINE runProxyCanvas #-}

guiGetSkin:: MonadIO m => Gui -> m Skin
guiGetSkin gui = guiSkin <$> readMonadIORef gui
{-# INLINE guiGetSkin #-}

getSkinFromWin:: MonadIO m => GuiWindow -> m Skin
getSkinFromWin = (=<<) guiGetSkin . getGuiFromWindow
{-# INLINE getSkinFromWin #-}

getSkinFromWidget:: MonadIO m => Widget -> m Skin
getSkinFromWidget = (=<<) getSkinFromWin . getWidgetWindow
{-# INLINE getSkinFromWidget #-}

guiGetResourceManager :: MonadIO m => Gui -> m ResourceManager
guiGetResourceManager gui = resourceManager <$> readMonadIORef gui
{-# INLINE guiGetResourceManager #-}

guiSetCursor :: MonadIO m => Gui -> CursorIx -> m ()
guiSetCursor gui ix = (`rmSetCursor` ix) =<< guiGetResourceManager gui
{-# INLINE guiSetCursor #-}

getShftCtrlAlt :: SDL.KeyModifier -> (Bool,Bool,Bool)
getShftCtrlAlt km = (SDL.keyModifierLeftShift km || SDL.keyModifierRightShift km,
                     SDL.keyModifierLeftCtrl  km || SDL.keyModifierRightCtrl km,
                     SDL.keyModifierLeftAlt km || SDL.keyModifierRightAlt km || SDL.keyModifierAltGr km)
{-# INLINE getShftCtrlAlt #-}

isEnterKey :: SDL.Keycode -> Bool
isEnterKey keycode = keycode == SDL.KeycodeReturn || -- keycode == SDL.KeycodeReturn2 ||
                     keycode == SDL.KeycodeCaret || keycode == SDL.KeycodeKPEnter
{-# INLINE isEnterKey #-}

clearFocusInWindow :: MonadIO m => GuiWindow -> m ()
clearFocusInWindow rfWin = do
    win <- readMonadIORef rfWin
    whenIsJust (focusedWidget win) $ \ widget -> do
        writeMonadIORef rfWin win{focusedWidget=Nothing}
        widgetFlagsRemove widget WidgetFocused
        markWidgetForRedraw widget

clearWidgetFocus :: MonadIO m => Widget -> m ()
clearWidgetFocus = (=<<) clearFocusInWindow .  getWidgetWindow
{-# INLINE clearWidgetFocus #-}

setWidgetFocus :: MonadIO m => Widget -> m ()
setWidgetFocus widget = do
    widg <- readMonadIORef widget
{-    sDbg <- widgetCoordsToStr widget
    liftIO $ putStrLn $ concat ["setWidgetFocus  ",sDbg," Focused=",
        show ((widgetFlags widg .&. WidgetFocused) /= WidgetNoFlags)] -}
    when ( (widgetFlags widg .&.
                (WidgetFocused .|. WidgetEnable .|. WidgetVisible .|. WidgetFocusable)) ==
                                (WidgetEnable .|. WidgetVisible .|. WidgetFocusable)) $ do
--        liftIO $ putStrLn "setWidgetFocus  pass"
        let rfWin = windowOfWidget widg
        win <- readMonadIORef rfWin
        whenIsJust (focusedWidget win) $ \ widget' -> do
{-            sDbgR <- widgetCoordsToStr widget'
            dbgFsd <- allWidgetFlags widget' WidgetFocused
            liftIO $ putStrLn $ concat ["setWidgetFocus : remove previous  ",sDbgR,
                " Focused=",show dbgFsd] -}
            widgetFlagsRemove widget' WidgetFocused
            markWidgetForRedraw widget'
        writeMonadIORef rfWin win{focusedWidget=Just widget}
        writeMonadIORef widget widg{widgetFlags=widgetFlags widg .|. WidgetFocused}
--        widgetFlagsAdd widget WidgetFocused
        markWidgetForRedraw widget

redrawAll :: MonadIO m => Gui -> m ()
redrawAll = allWindowsMap_ (`redrawWindow` False)
{-# INLINE redrawAll #-}

createWidget:: MonadIO m => Widget -> (Widget -> Skin -> m (GuiWidget a)) -> m (GuiWidget a)
createWidget parent initF = do
        win   <- getWidgetWindow parent
        gui   <- getGuiFromWindow win
        skin  <- guiGetSkin gui
        initF parent skin

mkWidget:: MonadIO m => WidgetFlags -> WidgetMargin -> a -> Widget -> WidgetFunctions -> m (GuiWidget a)
mkWidget fl marg a parent fs = do
        win   <- getWidgetWindow parent
        child <- mkWidget' win parent fl marg fs
        modifyMonadIORef' parent (\w -> w{cildrenWidgets= cildrenWidgets w `V.snoc` child})
        onCreate fs child
        return $ GuiWidget child a

data SimpleWidget = SimpleWidget

mkSimpleWidget:: MonadIO m => WidgetMargin -> Widget -> WidgetFunctions -> m (GuiWidget SimpleWidget)
mkSimpleWidget marg = mkWidget WidgetVisible marg SimpleWidget

-------------------------------------------------------------
delWidget:: MonadIO m => Widget -> m ()
delWidget widget = getWidgetWindow widget >>= go'
   where go' win = do
           parentRf <- getWidgetParent widget
           go widget
           parent <- readMonadIORef parentRf
           let chlds = cildrenWidgets parent
               pIx = V.elemIndex widget chlds
--           sDbg <- showWidgets widget Nothing
--           liftIO $ putStrLn $ concat ["delWidget  pIx=",show pIx,"   ", sDbg]
           case pIx of
               Just i -> do
                    writeMonadIORef parentRf parent{cildrenWidgets=unsafeDelElemByIx i chlds}
                    markWidgetForRedraw parentRf
               _ -> return ()
             where go widget' = do
                    w <- readMonadIORef widget'
                    (onDestroy $ widgetFns w) widget'
                    V.mapM_ go $ cildrenWidgets w
                    writeMonadIORef widget' $! w{cildrenWidgets=V.empty,parentWidget=widget'}
                    isSpec <- isSpecStateWidget win widget'
                    when isSpec $ modifyMonadIORef' win (\x -> x{specStateWidget=WidgetNoSpecState})
                    focusedWidg <- getFocusedWidget win
                    when (focusedWidg == Just widget') $ setFocusedWidget win Nothing
                    mm <- getWinMainMenu win
                    when (mm == Just widget') $ setWinMainMenu win Nothing
                    widgUC <- getWidgetUnderCursor win
                    when (widgUC == Just widget') $ setWidgetUnderCursor win Nothing
                    --winMainMenu

delAllChildWidgets  :: MonadIO m => Widget -> m ()
delAllChildWidgets widget = do
    w <- readMonadIORef widget
    V.mapM_ delWidget $ cildrenWidgets w
    writeMonadIORef widget w{cildrenWidgets=V.empty}


lastChildReplaceFirst :: MonadIO m => Widget -> m ()
lastChildReplaceFirst widget = do
    delW <- (V.head . cildrenWidgets) <$> readMonadIORef widget
--    liftIO $ putStrLn "lastChildReplaceFirst"
    delWidget delW
    modifyMonadIORef' widget (\x -> x{cildrenWidgets= moveLastToFirst $ cildrenWidgets x})

{-
setWinMainWidget :: MonadIO m => GuiWindow -> (Widget -> Skin -> m (GuiWidget a)) -> m (GuiWidget a)
setWinMainWidget rfWin initF = do
    mainW <- getWindowMainWidget rfWin
    delAllChildWidgets mainW
    createWidget mainW initF
-}
delWindowByIx:: MonadIO m => Gui -> GuiWindowIx -> m ()
delWindowByIx gui winIx = do
    gUI <- readMonadIORef gui
    let m = guiWindows gUI
    whenIsJust (Map.lookup winIx m) $ \ rfWin -> do
            win <- readMonadIORef rfWin
            delWidget $ mainWidget win
            SDL.destroyTexture $ winProxyTexture win
            SDL.destroyTexture $ winBuffer win
            SDL.destroyRenderer $ winRenderer win
            SDL.destroyWindow $ winSDL win
            writeMonadIORef gui $! gUI{guiWindows= Map.delete winIx m}

delWindow:: MonadIO m => GuiWindow -> m ()
delWindow rfWin = do
    win <- readMonadIORef rfWin
    delWindowByIx (guiOfWindow win) $ getWinIx' win

delAllWindows:: MonadIO m => Gui -> m ()
delAllWindows gui = do
    m <- getWindowsMap gui
    case Map.keys m of
        [] -> return ()
        ks -> forM_ ks (delWindowByIx gui) >> delAllWindows gui

