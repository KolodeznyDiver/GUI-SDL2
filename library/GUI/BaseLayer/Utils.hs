{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module GUI.BaseLayer.Utils(
    WidgetComposer(..),mulDiv,getWidgetCoordOffset,coordToWidget,mouseToWidget
    ,runProxyWinCanvas,runProxyCanvas,guiGetSkin,getSkinFromWin,getSkinFromWidget,guiGetResourceManager
    ,guiSetCursor,setGuiState,getGuiState,getUniqueCode,notifyEachWidgetsInAllWin
    ,clearFocusInWindow,clearWidgetFocus,setWidgetFocus
    ,redrawAll,forEachWidgetsInAllWin,createWidget,mkWidget,SimpleWidget(..),mkSimpleWidget
    ,delWidget,delAllChildWidgets,lastChildReplaceFirst
    ,delWindowByIx,delWindow,delAllWindows,delWindowsBy,windowsFold,getWindowsCount,showWindowsAsStr
    ,delAllPopupWindows
    ,markWidgetForRedraw,setWidgetFlag,notifyParentAboutSize,simpleOnResizing,simpleOnResizingMoveOnly
    ,extendableOnResizing,enableWidget,visibleWidget,newWindow',newWindow,overlapsChildrenFns,setWinSize',setWinSize
    ,guiApplicationExitSuccess,guiApplicationExitFailure,guiApplicationExitWithCode
                 ) where

import Control.Monad
import Control.Monad.IO.Class -- (MonadIO)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Data.Bits
import System.Exit
import Maybes (whenIsJust)
import Data.StateVar
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
import GUI.BaseLayer.Action

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

setGuiState :: MonadIO m => Gui -> GuiState -> m ()
setGuiState gui n = do
    g@GUIStruct{..} <- readMonadIORef gui
    when (guiState /= n) $ do
        writeMonadIORef gui g{guiState=n}
        forEachWidgetsInAllWin gui $ \widget -> do
            fns <- getWidgetFns widget
            onGuiStateChange fns widget n
{-# INLINE setGuiState #-}

getGuiState :: MonadIO m => Gui -> m GuiState
getGuiState = fmap guiState . readMonadIORef
{-# INLINE getGuiState #-}

notifyEachWidgetsInAllWin :: MonadIO m => Gui -> GuiNotifyCode -> Maybe Widget -> m ()
notifyEachWidgetsInAllWin gui nc mbW = forEachWidgetsInAllWin gui $ \widget -> do
                                            fns <- getWidgetFns widget
                                            onNotify fns widget nc mbW
{-# INLINE notifyEachWidgetsInAllWin #-}

getUniqueCode :: MonadIO m => Gui -> m UniqueCode
getUniqueCode gui = do
    g <- readMonadIORef gui
    return (UniqueCode $ guiUnique g) <* writeMonadIORef gui g{guiUnique= 1 + guiUnique g}
{-# INLINE getUniqueCode #-}

clearWidgetFocusInternal :: MonadIO m => Widget -> m ()
clearWidgetFocusInternal widget = widgetFlagsRemove widget WidgetFocused >> markWidgetForRedraw widget >>
    getWidgetFns widget >>= (`onLostKeyboardFocus` widget)
{-# INLINE clearWidgetFocusInternal #-}

clearFocusInWindow :: MonadIO m => GuiWindow -> m ()
clearFocusInWindow rfWin = do
    win <- readMonadIORef rfWin
    whenIsJust (focusedWidget win) $ \ widget -> do
        writeMonadIORef rfWin win{focusedWidget=Nothing}
        clearWidgetFocusInternal widget
{-        widgetFlagsRemove widget WidgetFocused
        fns <- getWidgetFns widget
        onLostKeyboardFocus fns widget
        markWidgetForRedraw widget -}

clearWidgetFocus :: MonadIO m => Widget -> m ()
clearWidgetFocus widget = do
    focused <- allWidgetFlags widget WidgetFocused
    when focused (clearFocusInWindow =<< getWidgetWindow widget)
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
        whenIsJust (focusedWidget win) $ \ widget' -> -- do
{-            sDbgR <- widgetCoordsToStr widget'
            dbgFsd <- allWidgetFlags widget' WidgetFocused
            liftIO $ putStrLn $ concat ["setWidgetFocus : remove previous  ",sDbgR,
                " Focused=",show dbgFsd] -}
            clearWidgetFocusInternal widget'
        modifyMonadIORef' rfWin (\x -> x{focusedWidget=Just widget})
        widgetFlagsAdd widget WidgetFocused
        markWidgetForRedraw widget
        onGainedKeyboardFocus (widgetFns widg) widget

forEachWidgetsInAllWin :: MonadIO m => Gui -> (Widget -> m ()) -> m ()
forEachWidgetsInAllWin gui f = allWindowsMap_ ( (=<<) (`forEachWidgets` f) . getWindowMainWidget) gui
{-# INLINE forEachWidgetsInAllWin #-}

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
                    modifyMonadIORef' widget' (\x -> x{cildrenWidgets=V.empty,parentWidget=widget'})
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
    modifyMonadIORef' widget (\x -> x{cildrenWidgets=V.empty})


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
    cWins' <- getWindowsCount gui
    m <- getWindowsMap gui
    whenIsJust (Map.lookup winIx m) $ \ rfWin -> do
            delWidget =<< getWindowMainWidget rfWin
            win <- readMonadIORef rfWin
            SDL.destroyTexture $ winProxyTexture win
            SDL.destroyTexture $ winBuffer win
            SDL.destroyRenderer $ winRenderer win
            SDL.destroyWindow $ winSDL win
            modifyMonadIORef' gui (\x -> x{guiWindows= Map.delete winIx $ guiWindows x})
            cWins <- getWindowsCount gui
            sDbg <- showWindowsAsStr gui
            liftIO $ putStrLn $ concat ["delWindowByIx cWins before = ",show cWins',
                 "   cWins after = ", show cWins, "   deleted = ",show $ winSDL win, "      ",sDbg]

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

delWindowsBy:: MonadIO m => Gui -> (GuiWindow -> m Bool) -> m ()
delWindowsBy gui predicate = do
    m <- getWindowsMap gui
    forM_ (Map.toList m) $ \ (winIx,win) -> do
        b <- predicate win
        when b $ delWindowByIx gui winIx

windowsFold:: MonadIO m => Gui -> (a -> GuiWindow -> m a) -> a -> m a
windowsFold gui f a = getWindowsMap gui >>= foldM f a . Map.elems
{-# INLINE windowsFold #-}

getWindowsCount :: MonadIO m => Gui -> m Int
getWindowsCount gui = Map.size <$> getWindowsMap gui
{-# INLINE getWindowsCount #-}

showWindowsAsStr :: MonadIO m => Gui -> m String
showWindowsAsStr gui = (show . Map.keys) <$> getWindowsMap gui

delAllPopupWindows :: MonadIO m => Gui -> m ()
delAllPopupWindows gui = delWindowsBy gui (`allWindowFlags` WindowPopupFlag)
{-# INLINE delAllPopupWindows #-}

--------------------------------------------------------------------------------------
markWidgetForRedraw :: MonadIO m => Widget -> m ()
markWidgetForRedraw widget = do
    w <- readMonadIORef widget
    when (not $ isWidgetMarkedForRedrawing' w) $ do
        onMarkForRedrawNotiy (widgetFns w) widget
        widgetFlagsAdd widget WidgetRedrawFlag
        windowFlagsAdd (windowOfWidget w) WindowRedrawFlag
{-# INLINE markWidgetForRedraw #-}

setWidgetFlag :: MonadIO m => WidgetFlags -> GuiWidget a -> Bool -> m ()
setWidgetFlag fl g ena = let widget = getWidget g in do
    o <- allWidgetFlags widget fl
    if ena then
        unless o $ do
            widgetFlagsAdd widget fl
            markWidgetForRedraw widget
    else when o $ do
            widgetFlagsRemove widget fl
            markWidgetForRedraw widget

-- no exported
notifyParentOnSizeChangedAndMarkForRedraw :: MonadIO m => Widget -> GuiSize -> m ()
notifyParentOnSizeChangedAndMarkForRedraw widget sz = do (parent,fs) <- getWidgetParentFns widget
                                                         markWidgetForRedraw parent
                                                         onSizeChangedParentNotify fs parent widget sz

notifyParentAboutSize :: MonadIO m => Widget -> GuiSize -> m ()
notifyParentAboutSize widget initSz = calcWidgetSizeWithMargin widget initSz >>=
            notifyParentOnSizeChangedAndMarkForRedraw widget
{-# INLINE notifyParentAboutSize #-}

simpleOnResizing' :: MonadIO m => (Widget -> GuiRect -> m GuiRect)
                                    -> Widget -> GuiRect -> m GuiRect
simpleOnResizing' recalcWithMargin widget newRect = do
    r@(SDL.Rectangle _ sz) <- recalcWithMargin widget newRect
    getWidgetParent widget >>=  markWidgetForRedraw
    setWidgetCanvasRect widget $ SDL.Rectangle zero sz
    return r

simpleOnResizing :: MonadIO m => Widget -> GuiRect -> m GuiRect
simpleOnResizing = simpleOnResizing' setWidgetRectWithMarginShrink
{-# INLINE simpleOnResizing #-}

simpleOnResizingMoveOnly :: MonadIO m => Widget -> GuiRect -> m GuiRect
simpleOnResizingMoveOnly = simpleOnResizing' setWidgetRectWithMarginShrinkMoveOnly
{-# INLINE simpleOnResizingMoveOnly #-}

extendableOnResizing :: MonadIO m => GuiSize -> Widget -> GuiRect -> m GuiRect
extendableOnResizing initSz@(V2 initW initH) widget newRect@(SDL.Rectangle _ sz) = do
    r <- simpleOnResizing widget newRect
    when (initW<0 || initH<0) $ notifyParentOnSizeChangedAndMarkForRedraw widget $ sizeRestoreNegative initSz sz
    return r
{-# INLINE extendableOnResizing #-}

enableWidget :: MonadIO m => GuiWidget a -> Bool -> m ()
enableWidget = setWidgetFlag WidgetEnable
{-# INLINE enableWidget #-}

visibleWidget :: MonadIO m => GuiWidget a -> Bool -> m ()
visibleWidget = setWidgetFlag WidgetEnable
{-# INLINE visibleWidget #-}

newWindow':: MonadIO m => Gui -> T.Text -> WindowFlags -> SDL.WindowConfig -> m GuiWindow
newWindow' rfGui winTitle winFl winCfg = do
    wSDL  <- SDL.createWindow winTitle winCfg
    rSDL  <- SDL.createRenderer wSDL (-1) SDL.defaultRenderer
    sz <- P.fromSDLV2 <$> get (SDL.windowSize wSDL)
    buf <- P.createTargetTexture rSDL sz
    proxyTexture <- P.createTargetTexture rSDL $ V2 1 1
    rfWin <- newMonadIORef WindowStruct  { guiOfWindow = rfGui
                                   , winSDL = wSDL
                                   , winRenderer = rSDL
                                   , mainWidget = undefined
                                   , winFlags = winFl
                                   , specStateWidget = WidgetNoSpecState
                                   , widgetUnderCursor = Nothing
                                   , focusedWidget = Nothing
                                   , curWinCursor = DefCursorIx
                                   , winBuffer = buf
                                   , winProxyTexture = proxyTexture
                                   --, winPrev = Nothing
                                   , winMainMenu = Nothing
                                   }
    rfW <- mkWidget' rfWin undefined WidgetVisible WidgetMarginNone (overlapsChildrenFns zero)
    setWidgetParent rfW rfW
    let rect = SDL.Rectangle zero sz
    setWidgetRect rfW rect
    setWidgetCanvasRect  rfW rect
    modifyMonadIORef' rfWin (\x -> x{mainWidget=rfW})
    modifyMonadIORef' rfGui (\x -> x{guiWindows= Map.insert wSDL rfWin $ guiWindows x})
    sDbg <- showWindowsAsStr rfGui
    liftIO $ putStrLn $ concat ["newWindow' new = ",show wSDL, "   ",sDbg]
    return rfWin

newWindow:: MonadIO m => Gui -> T.Text -> SDL.WindowConfig -> m GuiWindow
newWindow rfGui winTitle = newWindow' rfGui winTitle WindowRedrawFlag
{-# INLINE newWindow #-}

overlapsChildrenFns :: GuiSize -> WidgetFunctions
overlapsChildrenFns initInsideSz = defWidgFns{
    onCreate = \widget -> notifyParentAboutSize widget initInsideSz
    ,onSizeChangedParentNotify= \widget child _ -> getWidgetCanvasRect widget >>= widgetResizingIfChanged child
    ,onResizing= \widget newRect -> do
                r <- extendableOnResizing initInsideSz widget newRect
                mapByWidgetChildren_ (\c -> do {fs <- getWidgetFns c; onResizing fs c r}) widget
                             }

setWinSize' :: MonadIO m => GuiWindow -> GuiSize -> m ()
setWinSize' rfWin newSz = do
    widget <- getWindowMainWidget rfWin
    oldSz <- sizeOfRect <$> getWidgetRect widget
    when (newSz /= oldSz) $ do
        fs <- getWidgetFns widget
        onResizing fs widget $ SDL.Rectangle zero newSz
        markWidgetForRedraw widget

setWinSize :: MonadIO m => GuiWindow -> GuiSize -> m ()
setWinSize rfWin newSz = do
    winSDL <- getSDLWindow rfWin
    SDL.windowSize winSDL $= P.toSDLV2 newSz
    setWinSize' rfWin newSz
{-# INLINE setWinSize #-}

guiApplicationExitSuccess :: MonadIO m => Gui -> m ()
guiApplicationExitSuccess _gui = liftIO $ exitSuccess
{-# INLINE guiApplicationExitSuccess #-}

guiApplicationExitFailure :: MonadIO m => Gui -> m ()
guiApplicationExitFailure _gui = liftIO $ exitSuccess
{-# INLINE guiApplicationExitFailure #-}

guiApplicationExitWithCode :: MonadIO m => Gui -> Int -> m ()
guiApplicationExitWithCode gui 0 = guiApplicationExitSuccess gui
guiApplicationExitWithCode _gui code = liftIO $ exitWith $ ExitFailure code
{-# INLINE guiApplicationExitWithCode #-}