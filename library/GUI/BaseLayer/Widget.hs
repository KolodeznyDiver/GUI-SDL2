{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module GUI.BaseLayer.Widget(
     pattern WidgetNoFlags, pattern WidgetSelectable, pattern WidgetEnable, pattern WidgetVisible
    ,pattern WidgetFocusable,pattern WidgetTabbed, pattern WidgetMouseWheelControl,pattern WidgetFocused
    --,WidgetInit(..)
    ,GuiWidget(..),defWidgFns,getWidget,getWidgetData,getWidgetParent
    ,setWidgetParent,getWidgetRect,setWidgetRect,getWidgetCanvasRect,setWidgetCanvasRect,getWidgetVisibleRect
    ,getVisibleRect
    ,getWidgetMargin,setWidgetMargin,setWidgetMargin'
    ,getWidgetMarginSize,setWidgetRectWithMarginShrink,setWidgetRectWithMarginShrinkMoveOnly,
    getWidgetRectWithMargin,calcWidgetSizeWithMargin,notifyParentSizeWithMargin
    ,widgetCoordsToStr,simpleOnResizing,simpleOnResizingMoveOnly,showWidgets,showWidgetsFromMain
    --,toWidgetCanvasCoord,toCanvasCoord,getWidgetFsAndCoord
    ,getWidgetFlags,setWidgetFlags,widgetFlagsAddRemove
    ,widgetFlagsAdd,removeWidgetFlags,widgetFlagsRemove,allWidgetFlags',allWidgetFlags,anyWidgetFlags
    ,setWidgetFns,getWidgetFns,getWidgetParentFns,widgetResizingIfChanged
    ,getWidgetCursorIx,setWidgetCursorIx,getWidgetWindow
    ,isWidgetMarkedForRedrawing',isWidgetMarkedForRedrawing,clearWidgetRedrawFlag',clearWidgetRedrawFlag
    ,markWidgetForRedraw,setWidgetFlag,enableWidget,visibleWidget,getWidgetChildrenCount,getWidgetChild
    ,foldByWidgetChildren,foldByWidgetChildren',ifoldByWidgetChildren,ifoldByWidgetChildren'
    ,foldByWidgetChildren_,foldByWidgetChildren'_,ifoldByWidgetChildren_,ifoldByWidgetChildren'_
    ,mapByWidgetChildren,imapByWidgetChildren,mapByWidgetChildren_,imapByWidgetChildren_
    ,getChildWidgetIx,getWidgetParentIx,getPrevWidget,isMainWidget,getWinMainWidget
    ,findWidgetInTreeForward,findWidgetInTreeBackward,findNextTabbedWidget,findPrevTabbedWidget,mkWidget'
                 ) where

import Control.Monad.Trans.Class
import qualified SDL
import SDL.Vect
import Data.Bits
import qualified Data.Vector as V
import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import Control.Concurrent.STM
--import Control.Arrow
import GUI.BaseLayer.Types
import GUI.BaseLayer.Ref
import GUI.BaseLayer.BitFlags
import GUI.BaseLayer.Cursor
import GUI.BaseLayer.Internal.Types
import GUI.BaseLayer.Geometry
-- import GUI.BaseLayer.Primitives

pattern WidgetNoFlags :: WidgetFlags
pattern WidgetSelectable :: WidgetFlags
pattern WidgetEnable :: WidgetFlags
pattern WidgetVisible :: WidgetFlags
pattern WidgetFocusable :: WidgetFlags
pattern WidgetMouseWheelControl :: WidgetFlags
pattern WidgetTabbed :: WidgetFlags
pattern WidgetFocused :: WidgetFlags
-- pattern WidgetMarkForPaint :: WidgetFlags
                                    --  5432109876543210
pattern WidgetNoFlags        = (Flags 0x0000000000000000) :: WidgetFlags
pattern WidgetVisible        = (Flags 0x0000000000000001) :: WidgetFlags
pattern WidgetEnable         = (Flags 0x0000000000000002) :: WidgetFlags
pattern WidgetFocusable      = (Flags 0x0000000000000004) :: WidgetFlags
pattern WidgetMouseWheelControl      = (Flags 0x0000000000000008) :: WidgetFlags
pattern WidgetFocused          = (Flags 0x0000000000000010) :: WidgetFlags
{-
pattern WidgetResizableX     = (Flags 0x0000000000000004) :: WidgetFlags
pattern WidgetResizableY     = (Flags 0x0000000000000008) :: WidgetFlags
pattern WidgetMoveableX      = (Flags 0x0000000000000010) :: WidgetFlags
pattern WidgetMoveableY      = (Flags 0x0000000000000020) :: WidgetFlags
-}
pattern WidgetSelectable     = (Flags 0x0000000000000040) :: WidgetFlags

pattern WidgetTabbed         = (Flags 0x0000000000000100) :: WidgetFlags
{-
pattern WidgetClickable      = (Flags 0x0000000000000200) :: WidgetFlags
pattern WidgetDblClickable   = (Flags 0x0000000000000400) :: WidgetFlags
pattern WidgetCursorCaptured = (Flags 0x0000000000000800) :: WidgetFlags
pattern WidgetBordered       = (Flags 0x0000000000001000) :: WidgetFlags
-}
-- pattern WidgetMarkForPaint   = (Flags 0x0000000000002000) :: WidgetFlags
{-
pattern Widget = (Flags 0x0000000000000000) :: WidgetFlags
-}

-- data WidgetInit a = WidgetInit WidgetFlags WidgetMargin a WidgetFunctions
data GuiWidget a = GuiWidget Widget a

defWidgFns :: WidgetFunctions
defWidgFns = WidgetFunctions    { onCreate = (`notifyParentOnSizeChangedAndMarkForRedraw` zero)
                                , onDestroy = \_ -> return ()
                                , onDraw = \_ -> return ()
                                , onSizeChangedParentNotiy = \_ _ _ -> return ()
                                , onMarkForRedrawNotiy = \_ -> return ()
                                , onResizing = \_ _ -> return ()
                                , onGainedMouseFocus = \_ _ -> return ()
                                , onMouseMotion = \_ _ _ _ -> return ()
                                , onMouseButton = \_ _ _ _ _ -> return ()
                                , onMouseWheel = \_ _ _ -> return ()
                                , onLostMouseFocus = \_ -> return ()
                                , onGainedKeyboardFocus = \_ -> return ()
                                , onLostKeyboardFocus = \_ -> return ()
                                , onTextInput = \_ _ -> return ()
                                , onKeyboard = \_ _ _ _ _ -> return ()
                                }

getWidget :: GuiWidget a -> Widget
getWidget (GuiWidget w _) = w
{-# INLINE getWidget #-}

getWidgetData :: GuiWidget a -> a
getWidgetData (GuiWidget _ a) = a
{-# INLINE getWidgetData #-}

getWidgetParent:: MonadIO m => Widget -> m Widget
getWidgetParent = fmap parentWidget . readMonadIORef
{-# INLINE getWidgetParent #-}

setWidgetParent:: MonadIO m => Widget -> Widget -> m ()
setWidgetParent widget parent = modifyMonadIORef' widget (\w -> w{parentWidget=parent})
{-# INLINE setWidgetParent #-}

getWidgetRect:: MonadIO m => Widget -> m GuiRect
getWidgetRect = fmap widgetRect . readMonadIORef
{-# INLINE getWidgetRect #-}

setWidgetRect:: MonadIO m => Widget -> GuiRect -> m ()
setWidgetRect widget r = modifyMonadIORef' widget (\w -> w{widgetRect=r})
{-# INLINE setWidgetRect #-}

getWidgetVisibleRect :: MonadIO m => Widget -> m GuiRect
getWidgetVisibleRect widget = do
    w <- readMonadIORef widget
    return $ SDL.Rectangle ({- fmap negate $ -} pointOfRect $ widgetCanvasRect w) $ sizeOfRect $ widgetRect w

getVisibleRect :: MonadIO m => Widget -> GuiCanvas m GuiRect
getVisibleRect = lift . getWidgetVisibleRect
{-# INLINE getVisibleRect #-}

getWidgetCanvasRect:: MonadIO m => Widget -> m GuiRect
getWidgetCanvasRect = fmap widgetCanvasRect . readMonadIORef
{-# INLINE getWidgetCanvasRect #-}

setWidgetCanvasRect:: MonadIO m => Widget -> GuiRect -> m ()
setWidgetCanvasRect widget r = modifyMonadIORef' widget (\w -> w{widgetCanvasRect=r})
{-# INLINE setWidgetCanvasRect #-}

getWidgetMargin:: MonadIO m => Widget -> m GuiMargin
getWidgetMargin = fmap widgetMargin . readMonadIORef
{-# INLINE getWidgetMargin #-}

setWidgetMargin:: MonadIO m => Widget -> GuiMargin -> m ()
setWidgetMargin widget m = modifyMonadIORef' widget (\w -> w{widgetMargin=m})
{-# INLINE setWidgetMargin #-}

setWidgetMargin':: MonadIO m => Widget -> WidgetMargin -> m ()
setWidgetMargin' widget = setWidgetMargin widget . marginToLTRB
{-# INLINE setWidgetMargin' #-}

getWidgetMarginSize :: MonadIO m => Widget -> m GuiSize
getWidgetMarginSize = fmap marginSize . getWidgetMargin
{-# INLINE getWidgetMarginSize #-}

setWidgetRectWithMarginShrink :: MonadIO m => Widget -> GuiRect -> m GuiRect
setWidgetRectWithMarginShrink widget newRect =
    do r <- (`rectShrinkByMargin` newRect) <$> getWidgetMargin widget
       setWidgetRect widget r
       return r
{-# INLINE setWidgetRectWithMarginShrink #-}

setWidgetRectWithMarginShrinkMoveOnly :: MonadIO m => Widget -> GuiRect -> m GuiRect
setWidgetRectWithMarginShrinkMoveOnly widget newRect =
    do p <- (pointOfRect . (`rectShrinkByMargin` newRect)) <$> getWidgetMargin widget
       sz <- sizeOfRect <$> getWidgetRect widget
       let r = SDL.Rectangle p sz
       setWidgetRect widget r
       return r
{-# INLINE setWidgetRectWithMarginShrinkMoveOnly #-}

getWidgetRectWithMargin :: MonadIO m => Widget -> m GuiRect
getWidgetRectWithMargin widget = do
    w <- readMonadIORef widget
    return $ rectGrowByMargin (widgetMargin w) $ widgetRect w
{-# INLINE getWidgetRectWithMargin #-}

calcWidgetSizeWithMargin :: MonadIO m => Widget -> GuiSize -> m GuiSize
calcWidgetSizeWithMargin widget initSz = do
    mSz <- getWidgetMarginSize widget
    return $ (\i m -> if i<=0 then i else i+m) <$> initSz <*> mSz
{-# INLINE calcWidgetSizeWithMargin #-}

-- no exported
notifyParentOnSizeChangedAndMarkForRedraw :: MonadIO m => Widget -> GuiSize -> m ()
notifyParentOnSizeChangedAndMarkForRedraw widget sz = do (parent,fs) <- getWidgetParentFns widget
                                                         markWidgetForRedraw parent
                                                         onSizeChangedParentNotiy fs parent widget sz

notifyParentSizeWithMargin :: MonadIO m => Widget -> GuiSize -> m ()
notifyParentSizeWithMargin widget initSz = calcWidgetSizeWithMargin widget initSz >>=
            notifyParentOnSizeChangedAndMarkForRedraw widget
{-# INLINE notifyParentSizeWithMargin #-}

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

widgetCoordsToStr:: MonadIO m => Widget -> m String
widgetCoordsToStr widget = do
    rect <- getWidgetRect widget
    canv <- getWidgetCanvasRect widget
    marg <- getWidgetMargin widget
    return $ concat ["rect=", rectToBriefStr rect, "  canv=", rectToBriefStr canv,
     "  marg=", marginToBriefStr marg ]


showWidgets :: MonadIO m => Widget -> Maybe Widget -> m String
showWidgets widget markedWidget = do
    sc <- foldByWidgetChildren' (\s w -> do { n <- showWidgets w markedWidget; return $ concat [s,'{':n,"}"]})
        "  chlds=[" widget
    s <- widgetCoordsToStr widget
    return $ concat [if Just widget == markedWidget then "@" else "",s,sc,"]"]

showWidgetsFromMain :: MonadIO m => Widget -> m String
showWidgetsFromMain widget = do
    mainWidg <- getWinMainWidget widget
    showWidgets mainWidg $ Just widget

{-
toCanvasCoord :: GuiPoint -> GuiPoint {- -> GuiMargin -} -> GuiPoint -> GuiPoint
toCanvasCoord  rectP canvP {- marg -} srcP = srcP ^-^ rectP ^+^ canvP -- .-^ (marginToLT marg)
{-# INLINE toCanvasCoord #-}

toWidgetCanvasCoord :: MonadIO m => Widget -> GuiPoint -> m GuiPoint
toWidgetCanvasCoord widget p = do
    w <- readMonadIORef widget
    return $ toCanvasCoord (pointOfRect $ widgetRect w) (pointOfRect $ widgetCanvasRect w) {- (widgetMargin w)  -} p
{-# INLINE toWidgetCanvasCoord #-}

getWidgetFsAndCoord  :: MonadIO m => Widget -> GuiPoint -> m (WidgetFunctions,GuiPoint)
getWidgetFsAndCoord widget p = do
    w <- readMonadIORef widget
    return $ (widgetFns w,toCanvasCoord (pointOfRect $ widgetRect w) (pointOfRect $ widgetCanvasRect w) {- (widgetMargin w) -} p)
{-# INLINE getWidgetFsAndCoord #-}
-}
removeWidgetFlags:: WidgetFlags -> WidgetFlags -> WidgetFlags
removeWidgetFlags fl rmv = fl .&. complement rmv
{-# INLINE removeWidgetFlags #-}

getWidgetFlags:: MonadIO m => Widget -> m WidgetFlags
getWidgetFlags = fmap widgetFlags .  readMonadIORef
{-# INLINE getWidgetFlags #-}

setWidgetFlags:: MonadIO m => Widget -> WidgetFlags -> m ()
setWidgetFlags widget fl = modifyMonadIORef' widget (\x -> x{widgetFlags=fl})
{-# INLINE setWidgetFlags #-}

widgetFlagsAddRemove:: MonadIO m => Widget -> WidgetFlags -> WidgetFlags -> m ()
widgetFlagsAddRemove widget add rmv =
    modifyMonadIORef' widget (\x -> x{widgetFlags=(widgetFlags x .&. complement rmv) .|. add})
{-# INLINE widgetFlagsAddRemove #-}

widgetFlagsAdd:: MonadIO m => Widget -> WidgetFlags -> m ()
widgetFlagsAdd widget add = modifyMonadIORef' widget (\x -> x{widgetFlags=widgetFlags x .|. add})
{-# INLINE widgetFlagsAdd #-}

widgetFlagsRemove:: MonadIO m => Widget -> WidgetFlags -> m ()
widgetFlagsRemove widget rmv = modifyMonadIORef' widget (\x -> x{widgetFlags=widgetFlags x .&. complement rmv})
{-# INLINE widgetFlagsRemove #-}

allWidgetFlags':: WidgetStruct -> WidgetFlags -> Bool
allWidgetFlags' w fl = fl == (fl .&. widgetFlags w)
{-# INLINE allWidgetFlags' #-}

allWidgetFlags:: MonadIO m => Widget -> WidgetFlags -> m Bool
allWidgetFlags widget fl = (`allWidgetFlags'` fl) <$> readMonadIORef widget
{-# INLINE allWidgetFlags #-}

anyWidgetFlags:: MonadIO m => Widget -> WidgetFlags -> m Bool
anyWidgetFlags widget fl = ((WidgetNoFlags /=) . (fl .&.) . widgetFlags) <$> readMonadIORef widget
{-# INLINE anyWidgetFlags #-}
{-
getWidgetFun:: MonadIO m => Widget -> m WidgetFun -- GHC doesn't yet support impredicative polymorphism
getWidgetFun = fmap widgetFun . readMonadIORef
{-# INLINE getWidgetFun #-}
-}
setWidgetFns:: MonadIO m => Widget -> WidgetFunctions -> m ()
setWidgetFns widget fs = modifyMonadIORef' widget (\w -> w{widgetFns=fs})
{-# INLINE setWidgetFns #-}

getWidgetFns:: MonadIO m => Widget -> m WidgetFunctions
getWidgetFns = fmap widgetFns . readMonadIORef
{-# INLINE getWidgetFns #-}

getWidgetParentFns:: MonadIO m => Widget -> m (Widget,WidgetFunctions)
getWidgetParentFns = (=<<) (\p -> (p,) <$> getWidgetFns p) . getWidgetParent
{-# INLINE getWidgetParentFns #-}

widgetResizingIfChanged :: MonadIO m => Widget -> GuiRect -> m ()
widgetResizingIfChanged widget newRect = do
    oldRect <- getWidgetRect widget
    r <- (`rectShrinkByMargin` newRect) <$> getWidgetMargin widget
    when (r /= oldRect) $ do
        fs <- getWidgetFns widget
        onResizing fs widget newRect

getWidgetCursorIx:: MonadIO m => Widget -> m CursorIx
getWidgetCursorIx = fmap widgetCursor . readMonadIORef
{-# INLINE getWidgetCursorIx #-}

setWidgetCursorIx:: MonadIO m => Widget -> CursorIx -> m ()
setWidgetCursorIx widget ix = modifyMonadIORef' widget (\w -> w{widgetCursor=ix})
{-# INLINE setWidgetCursorIx #-}

getWidgetWindow:: MonadIO m => Widget -> m GuiWindow
getWidgetWindow = fmap windowOfWidget . readMonadIORef
{-# INLINE getWidgetWindow #-}

isWidgetMarkedForRedrawing':: MonadIO m => WidgetStruct -> m Bool
isWidgetMarkedForRedrawing' = readTVarMonadIO . widgetRedrawFlag
{-# INLINE isWidgetMarkedForRedrawing' #-}

isWidgetMarkedForRedrawing:: MonadIO m => Widget -> m Bool
isWidgetMarkedForRedrawing = (=<<) isWidgetMarkedForRedrawing' . readMonadIORef
{-# INLINE isWidgetMarkedForRedrawing #-}

clearWidgetRedrawFlag':: MonadIO m => WidgetStruct -> m ()
clearWidgetRedrawFlag' w = atomicallyMonadIO $ writeTVar (widgetRedrawFlag w) False
{-# INLINE clearWidgetRedrawFlag' #-}

clearWidgetRedrawFlag:: MonadIO m => Widget -> m ()
clearWidgetRedrawFlag widget = readMonadIORef widget >>= clearWidgetRedrawFlag'
{-# INLINE clearWidgetRedrawFlag #-}

markWidgetForRedraw :: MonadIO m => Widget -> m ()
markWidgetForRedraw widget = do
    w <- readMonadIORef widget
    onMarkForRedrawNotiy (widgetFns w) widget
    win <- readMonadIORef $ windowOfWidget w
    atomicallyMonadIO (writeTVar (widgetRedrawFlag w) True >> writeTVar (winRedrawFlag win) True)
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

enableWidget :: MonadIO m => GuiWidget a -> Bool -> m ()
enableWidget = setWidgetFlag WidgetEnable
{-# INLINE enableWidget #-}

visibleWidget :: MonadIO m => GuiWidget a -> Bool -> m ()
visibleWidget = setWidgetFlag WidgetEnable
{-# INLINE visibleWidget #-}



getWidgetChildrenCount :: MonadIO m => Widget -> m Int
getWidgetChildrenCount = fmap ( V.length . cildrenWidgets) . readMonadIORef
{-# INLINE getWidgetChildrenCount #-}

getWidgetChild :: MonadIO m => Widget -> Int -> m (Maybe Widget)
getWidgetChild widget ix = ((V.!? ix) . cildrenWidgets) <$> readMonadIORef widget
{-# INLINE getWidgetChild #-}

foldByWidgetChildren:: MonadIO m => (a -> Widget -> m a) -> a -> Widget -> m a
foldByWidgetChildren f a wRef = V.foldM f a =<< (cildrenWidgets <$> readMonadIORef wRef)
{-# INLINE foldByWidgetChildren #-}

foldByWidgetChildren':: MonadIO m => (a -> Widget -> m a) -> a -> Widget -> m a
foldByWidgetChildren' f a wRef = V.foldM' f a =<< (cildrenWidgets <$> readMonadIORef wRef)
{-# INLINE foldByWidgetChildren' #-}

ifoldByWidgetChildren:: MonadIO m => (a -> Int -> Widget -> m a) -> a -> Widget -> m a
ifoldByWidgetChildren f a wRef = V.ifoldM f a =<< (cildrenWidgets <$> readMonadIORef wRef)
{-# INLINE ifoldByWidgetChildren #-}

ifoldByWidgetChildren':: MonadIO m => (a -> Int -> Widget -> m a) -> a -> Widget -> m a
ifoldByWidgetChildren' f a wRef = V.ifoldM' f a =<< (cildrenWidgets <$> readMonadIORef wRef)
{-# INLINE ifoldByWidgetChildren' #-}

foldByWidgetChildren_:: MonadIO m => (a -> Widget -> m a) -> a -> Widget -> m ()
foldByWidgetChildren_ f a wRef = V.foldM_ f a =<< (cildrenWidgets <$> readMonadIORef wRef)
{-# INLINE foldByWidgetChildren_ #-}

foldByWidgetChildren'_:: MonadIO m => (a -> Widget -> m a) -> a -> Widget -> m ()
foldByWidgetChildren'_ f a wRef = V.foldM'_ f a =<< (cildrenWidgets <$> readMonadIORef wRef)
{-# INLINE foldByWidgetChildren'_ #-}

ifoldByWidgetChildren_:: MonadIO m => (a -> Int -> Widget -> m a) -> a -> Widget -> m ()
ifoldByWidgetChildren_ f a wRef = V.ifoldM_ f a =<< (cildrenWidgets <$> readMonadIORef wRef)
{-# INLINE ifoldByWidgetChildren_ #-}

ifoldByWidgetChildren'_:: MonadIO m => (a -> Int -> Widget -> m a) -> a -> Widget -> m ()
ifoldByWidgetChildren'_ f a wRef = V.ifoldM'_ f a =<< (cildrenWidgets <$> readMonadIORef wRef)
{-# INLINE ifoldByWidgetChildren'_ #-}

mapByWidgetChildren:: MonadIO m => (Widget -> m a) -> Widget -> m (V.Vector a)
mapByWidgetChildren f wRef = V.mapM f =<< (cildrenWidgets <$> readMonadIORef wRef)
{-# INLINE mapByWidgetChildren #-}

imapByWidgetChildren:: MonadIO m => (Int -> Widget -> m a) -> Widget -> m (V.Vector a)
imapByWidgetChildren f wRef = V.imapM f =<< (cildrenWidgets <$> readMonadIORef wRef)
{-# INLINE imapByWidgetChildren #-}

mapByWidgetChildren_:: MonadIO m => (Widget -> m a) -> Widget -> m ()
mapByWidgetChildren_ f wRef = V.mapM_ f =<< (cildrenWidgets <$> readMonadIORef wRef)
{-# INLINE mapByWidgetChildren_ #-}

imapByWidgetChildren_:: MonadIO m => (Int -> Widget -> m a) -> Widget -> m ()
imapByWidgetChildren_ f wRef = V.imapM_ f =<< (cildrenWidgets <$> readMonadIORef wRef)
{-# INLINE imapByWidgetChildren_ #-}

getChildWidgetIx' :: WidgetStruct ->  Widget -> Maybe Int
getChildWidgetIx' w whatFound = V.findIndex (whatFound ==) $ cildrenWidgets w
{-# INLINE getChildWidgetIx' #-}

getChildWidgetIx :: MonadIO m =>  Widget ->  Widget -> m (Maybe Int)
getChildWidgetIx widget whatFound = (`getChildWidgetIx'` whatFound) <$> readMonadIORef widget
{-# INLINE getChildWidgetIx #-}

getWidgetParentIx :: MonadIO m => Widget -> m (Maybe Int)
getWidgetParentIx widget = do parent <- getWidgetParent widget
                              v <- cildrenWidgets <$> readMonadIORef parent
                              return $ V.findIndex (widget ==) v
{-# INLINE getWidgetParentIx #-}

getPrevWidget :: MonadIO m => Widget -> m (Maybe Widget)
getPrevWidget widget = do parent <- getWidgetParent widget
                          v <- cildrenWidgets <$> readMonadIORef parent
                          return (case V.findIndex (widget ==) v of
                                    Just i -> v V.!? (i - 1)
                                    _ -> Nothing)
{-# INLINE getPrevWidget #-}

isMainWidget:: MonadIO m =>  Widget -> m Bool
isMainWidget widget = ((widget==).parentWidget) <$> readMonadIORef widget
{-# INLINE isMainWidget #-}

getWinMainWidget:: MonadIO m => Widget -> m Widget
getWinMainWidget = (=<<) (fmap mainWidget . readMonadIORef) . getWidgetWindow

findWidgetInTreeForward :: MonadIO m => (Widget -> WidgetFlags -> m Bool) -> Widget -> m Widget
findWidgetInTreeForward predicate startWidget = do
    startParent <- getWidgetParent startWidget
    startStruct <- readMonadIORef startParent
    rootWidget  <- getWinMainWidget startWidget
    let go w s ix needToRoot =
            case cildrenWidgets s V.!? ix of
                Just w' | w' == startWidget -> return $ Just w'
                        | otherwise -> do
                            s' <- readMonadIORef w'
                            b <- predicate w' (widgetFlags s')
                            if b then return $ Just w'
                            else do r <- go w' s' 0 False
                                    case r of
                                        Nothing -> go w s (ix+1) needToRoot
                                        res -> return res
                _ | needToRoot ->
                        let parent = parentWidget s in
                        if rootWidget == parent then go w s 0 False
                        else do s' <- readMonadIORef parent
                                case getChildWidgetIx' s' w of
                                    Just i -> go parent s' (i+1) True
                                    _ -> error "findWidgetInTreeForward : widget not found in child vector"
                  | otherwise -> return Nothing
        startIx = case getChildWidgetIx' startStruct startWidget of
                        Just i -> i+1
                        _ -> error "findWidgetInTreeForward : start widget not found in child vector"
    r <- go startParent startStruct startIx True
    case r of
        Nothing -> error "findWidgetInTreeForward : widget not found in widget's tree"
        Just res -> return res

findWidgetInTreeBackward :: MonadIO m => (Widget -> WidgetFlags -> m Bool) -> Widget -> m Widget
findWidgetInTreeBackward predicate startWidget = do
    startParent <- getWidgetParent startWidget
    startStruct <- readMonadIORef startParent
    rootWidget  <- getWinMainWidget startWidget
    let lastIx s = V.length (cildrenWidgets s) - 1
        go w s (-1) True =
                        let parent = parentWidget s in
                        if rootWidget == parent then go w s (lastIx s) False
                        else do s' <- readMonadIORef parent
                                case getChildWidgetIx' s' w of
                                    Just i -> go parent s' (i-1) True
                                    _ -> error "findWidgetInTreeBackward : widget not found in child vector"
        go _ _ (-1) _ = return Nothing
        go w s ix needToRoot =
            let w' = cildrenWidgets s `V.unsafeIndex` ix in
            if w' == startWidget then return $ Just w'
            else do
                            s' <- readMonadIORef w'
                            b <- predicate w' (widgetFlags s')
                            if b then return $ Just w'
                            else do r <- go w' s' (lastIx s') False
                                    case r of
                                        Nothing -> go w s (ix-1) needToRoot
                                        res -> return res
        startIx = case getChildWidgetIx' startStruct startWidget of
                        Just i -> i-1
                        _ -> error "findWidgetInTreeBackward : start widget not found in child vector"
    r <- go startParent startStruct startIx True
    case r of
        Nothing -> error "findWidgetInTreeBackward : widget not found in widget's tree"
        Just res -> return res

isTabbedInternalPredicate :: MonadIO m => Widget -> WidgetFlags -> m Bool
isTabbedInternalPredicate _ fl = let f = WidgetEnable .|. WidgetVisible .|. WidgetFocusable .|. WidgetTabbed
                in return $ (fl .&. f) == f
{-# INLINE isTabbedInternalPredicate #-}

findNextTabbedWidget :: MonadIO m => Widget -> m Widget
findNextTabbedWidget = findWidgetInTreeForward isTabbedInternalPredicate
{-# INLINE findNextTabbedWidget #-}

findPrevTabbedWidget :: MonadIO m => Widget -> m Widget
findPrevTabbedWidget = findWidgetInTreeBackward isTabbedInternalPredicate
{-# INLINE findPrevTabbedWidget #-}

mkWidget':: MonadIO m => GuiWindow -> Widget -> WidgetFlags -> WidgetMargin -> WidgetFunctions -> m Widget
mkWidget' win parent fl marg fs = do
        rdrf <- newTVarMonadIO $ (fl .&. WidgetVisible) == WidgetVisible
        newMonadIORef WidgetStruct { windowOfWidget = win
                             , parentWidget = parent
                             , cildrenWidgets = V.empty
                             , widgetRect = SDL.Rectangle zero zero
                             , widgetCanvasRect = SDL.Rectangle zero zero
                             , widgetMargin = marginToLTRB marg
                             , widgetFlags = fl
                             , widgetRedrawFlag = rdrf
                             , widgetCursor = DefCursorIx
                             , widgetFns = fs
                             }
