{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
module GUI.BaseLayer.Window(
     pattern WindowNoFlags,pattern WindowCloseOnLostFocuse,pattern WindowWaitAlt
    ,getWinId'',getWinId',getWinId,getWinIx',getWinIx
    ,removeWindowFlags,getWindowFlags,setWindowFlags,windowFlagsAddRemove,windowFlagsAdd
    ,windowFlagsRemove,allWindowFlags',allWindowFlags,anyWindowFlags
    ,getSDLWindow,getWindowRenderer
    ,getWindowByIx,getFocusedWidget,setFocusedWidget,getWidgetUnderCursor,setWidgetUnderCursor
    ,getWinCursorIx,setWinCursorIx
    ,showWinWidgets,newWindow,getGuiFromWindow,getWindowMainWidget,getWindowsMap
    ,doForWinByIx,allWindowsMap_,redrawWindowByIx,redrawWindow,isSpecStateWidget
    ,resetSpecStateWidget,setSpecStateWidget,setMouseCapturedWidget,getMouseCapturedWidget,resetMouseCaptured
    ,resetMouseCapturedWidget,setWinMainMenu,getWinMainMenu,setWinNext,getWinNext
                 ) where

import qualified SDL
import qualified SDL.Raw as Raw
import qualified SDL.Internal.Types
import SDL.Vect
import Data.StateVar
import Data.Bits
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import Control.Monad
import Control.Concurrent.STM
--import Data.Maybe
import MonadUtils (whenM)
import Maybes (whenIsJust)
import GUI.BaseLayer.Types
import GUI.BaseLayer.Ref
import GUI.BaseLayer.BitFlags
import GUI.BaseLayer.Internal.Types
import GUI.BaseLayer.Widget
import GUI.BaseLayer.Cursor
import qualified GUI.BaseLayer.Primitives as P
import GUI.BaseLayer.Canvas
import GUI.BaseLayer.Handlers
import GUI.BaseLayer.Geometry

pattern WindowNoFlags :: WindowFlags
pattern WindowCloseOnLostFocuse :: WindowFlags
pattern WindowWaitAlt :: WindowFlags
                                    --  5432109876543210
pattern WindowNoFlags        = (Flags 0x0000000000000000) -- :: WindowFlags
pattern WindowCloseOnLostFocuse = (Flags 0x0000000000000001) :: WindowFlags
pattern WindowWaitAlt = (Flags 0x0000000000000002) :: WindowFlags
{-
pattern Window     = (Flags 0x0000000000000001) :: WindowFlags
-}

getSDLRawWindow':: SDL.Window -> Raw.Window
getSDLRawWindow' (SDL.Internal.Types.Window w) = w
{-# INLINE getSDLRawWindow' #-}

getSDLRawWindow:: WindowStruct -> Raw.Window
getSDLRawWindow = getSDLRawWindow' . winSDL
{-# INLINE getSDLRawWindow #-}

getSDLWindow:: MonadIO m => GuiWindow -> m SDL.Window
getSDLWindow rfWin = winSDL <$> readMonadIORef rfWin
{-# INLINE getSDLWindow #-}

getWindowRenderer:: MonadIO m => GuiWindow -> m SDL.Renderer
getWindowRenderer rfWin = winRenderer <$> readMonadIORef rfWin
{-# INLINE getWindowRenderer #-}

getWinId'':: MonadIO m => SDL.Window -> m GuiWindowId
getWinId'' = Raw.getWindowID . getSDLRawWindow'
{-# INLINE getWinId'' #-}

getWinId':: MonadIO m => WindowStruct -> m GuiWindowId
getWinId' = Raw.getWindowID . getSDLRawWindow
{-# INLINE getWinId' #-}

getWinId:: MonadIO m => GuiWindow -> m GuiWindowId
getWinId rfWin = getWinId' =<< readMonadIORef rfWin
{-# INLINE getWinId #-}

getWinIx':: WindowStruct -> GuiWindowIx
getWinIx' = winSDL
{-# INLINE getWinIx' #-}

getWinIx:: MonadIO m => GuiWindow -> m GuiWindowIx
getWinIx rfWin = getWinIx' <$> readMonadIORef rfWin
{-# INLINE getWinIx #-}
-----------------------------------------------------------------
removeWindowFlags:: WindowFlags -> WindowFlags -> WindowFlags
removeWindowFlags fl rmv = fl .&. complement rmv
{-# INLINE removeWindowFlags #-}

getWindowFlags:: MonadIO m => GuiWindow -> m WindowFlags
getWindowFlags = fmap winFlags .  readMonadIORef
{-# INLINE getWindowFlags #-}

setWindowFlags:: MonadIO m => GuiWindow -> WindowFlags -> m ()
setWindowFlags win fl = modifyMonadIORef' win (\x -> x{winFlags=fl})
{-# INLINE setWindowFlags #-}

windowFlagsAddRemove:: MonadIO m => GuiWindow -> WindowFlags -> WindowFlags -> m ()
windowFlagsAddRemove win add rmv =
    modifyMonadIORef' win (\x -> x{winFlags=(winFlags x .&. complement rmv) .|. add})
{-# INLINE windowFlagsAddRemove #-}

windowFlagsAdd:: MonadIO m => GuiWindow -> WindowFlags -> m ()
windowFlagsAdd win add = modifyMonadIORef' win (\x -> x{winFlags=winFlags x .|. add})
{-# INLINE windowFlagsAdd #-}

windowFlagsRemove:: MonadIO m => GuiWindow -> WindowFlags -> m ()
windowFlagsRemove win rmv = modifyMonadIORef' win (\x -> x{winFlags=winFlags x .&. complement rmv})
{-# INLINE windowFlagsRemove #-}

allWindowFlags':: WindowStruct -> WindowFlags -> Bool
allWindowFlags' w fl = fl == (fl .&. winFlags w)
{-# INLINE allWindowFlags' #-}

allWindowFlags:: MonadIO m => GuiWindow -> WindowFlags -> m Bool
allWindowFlags win fl = (`allWindowFlags'` fl) <$> readMonadIORef win
{-# INLINE allWindowFlags #-}

anyWindowFlags:: MonadIO m => GuiWindow -> WindowFlags -> m Bool
anyWindowFlags win fl = ((WindowNoFlags /=) . (fl .&.) . winFlags) <$> readMonadIORef win
{-# INLINE anyWindowFlags #-}
-----------------------------------------------------------------
showWinWidgets :: MonadIO m => GuiWindow -> Maybe Widget -> m String
showWinWidgets rfWin markedWidget = getWindowMainWidget rfWin >>= (`showWidgets` markedWidget)

newWindow:: MonadIO m => Gui -> T.Text -> SDL.WindowConfig -> m GuiWindow
newWindow rfGui winTitle winCfg = do
    wSDL  <- SDL.createWindow winTitle winCfg
    rSDL  <- SDL.createRenderer wSDL (-1) SDL.defaultRenderer
    sz <- P.fromSDLV2 <$> get (SDL.windowSize wSDL)
    buf <- P.createTargetTexture rSDL sz
    proxyTexture <- P.createTargetTexture rSDL $ V2 1 1
    rdrf <- newTVarMonadIO True
    rfWin <- newMonadIORef WindowStruct  { guiOfWindow = rfGui
                                   , winSDL = wSDL
                                   , winRenderer = rSDL
                                   , mainWidget = undefined
                                   , winFlags = WindowNoFlags
                                   , specStateWidget = WidgetNoSpecState
                                   , widgetUnderCursor = Nothing
                                   , focusedWidget = Nothing
                                   , curWinCursor = DefCursorIx
                                   , winBuffer = buf
                                   , winProxyTexture = proxyTexture
                                   , winRedrawFlag = rdrf
                                   , winNext = Nothing
                                   , winMainMenu = Nothing
                                   }
    rfW <- mkWidget' rfWin undefined WidgetVisible WidgetMarginNone oneChildFns
    setWidgetParent rfW rfW
    let rect = SDL.Rectangle zero sz
    setWidgetRect rfW rect
    setWidgetCanvasRect  rfW rect
    modifyMonadIORef' rfWin (\x -> x{mainWidget=rfW})
    modifyMonadIORef' rfGui (\x -> x{guiWindows= Map.insert wSDL rfWin $ guiWindows x})
    return rfWin

getGuiFromWindow :: MonadIO m => GuiWindow -> m Gui
getGuiFromWindow rfWin = guiOfWindow <$> readMonadIORef rfWin
{-# INLINE getGuiFromWindow #-}

getWindowMainWidget :: MonadIO m => GuiWindow -> m Widget
getWindowMainWidget rfWin = mainWidget <$> readMonadIORef rfWin
{-# INLINE getWindowMainWidget #-}

getWindowsMap :: MonadIO m => Gui -> m GuiWindowCollection
getWindowsMap gui = guiWindows <$> readMonadIORef gui
{-# INLINE getWindowsMap #-}

getWindowByIx :: MonadIO m => Gui -> GuiWindowIx -> m (Maybe GuiWindow)
getWindowByIx gui ix = (Map.lookup ix . guiWindows) <$> readMonadIORef gui
{-# INLINE getWindowByIx #-}

getFocusedWidget :: MonadIO m => GuiWindow -> m (Maybe Widget)
getFocusedWidget = fmap focusedWidget . readMonadIORef
{-# INLINE getFocusedWidget #-}

setFocusedWidget :: MonadIO m => GuiWindow -> Maybe Widget -> m ()
setFocusedWidget rfWin mb = modifyMonadIORef' rfWin (\x -> x{focusedWidget=mb})
{-# INLINE setFocusedWidget #-}

getWidgetUnderCursor :: MonadIO m => GuiWindow -> m (Maybe Widget)
getWidgetUnderCursor = fmap widgetUnderCursor . readMonadIORef
{-# INLINE getWidgetUnderCursor #-}

setWidgetUnderCursor :: MonadIO m => GuiWindow -> Maybe Widget -> m ()
setWidgetUnderCursor rfWin mb = modifyMonadIORef' rfWin (\x -> x{widgetUnderCursor=mb})
{-# INLINE setWidgetUnderCursor #-}

getWinCursorIx :: MonadIO m => GuiWindow -> m CursorIx
getWinCursorIx = fmap curWinCursor . readMonadIORef
{-# INLINE getWinCursorIx #-}

setWinCursorIx :: MonadIO m => GuiWindow -> CursorIx -> m ()
setWinCursorIx rfWin ix = modifyMonadIORef' rfWin (\x -> x{curWinCursor=ix})
{-# INLINE setWinCursorIx #-}


doForWinByIx:: MonadIO m => (GuiWindow -> m ()) -> Gui -> GuiWindowIx -> m ()
doForWinByIx f gui winIx = (`whenIsJust` f) =<< Map.lookup winIx <$> getWindowsMap gui

allWindowsMap_:: MonadIO m => (GuiWindow -> m ()) -> Gui -> m ()
allWindowsMap_ f gui = mapM_ f =<< getWindowsMap gui
{-# INLINE allWindowsMap_ #-}

redrawWindowByIx:: MonadIO m => Gui -> GuiWindowIx -> Bool -> m ()
redrawWindowByIx gui ix force = doForWinByIx (`redrawWindow` force) gui ix
{-# INLINE redrawWindowByIx #-}

redrawWindow:: MonadIO m => GuiWindow -> Bool -> m ()
redrawWindow rfWin force = do
    win <- readMonadIORef rfWin
    wSz <- P.fromSDLV2 <$> get (SDL.windowSize $ winSDL win)
    tSz <- P.getTextureSize $ winBuffer win
    let szChanged = wSz /= tSz
        force2 = force || szChanged
    whenM (if force2 then return True else readTVarMonadIO $ winRedrawFlag win) $ do
        atomicallyMonadIO $ writeTVar (winRedrawFlag win) False
        gui <- readMonadIORef $ guiOfWindow win
--        liftIO $ putStrLn $ "redrawWindow : wSz=" ++ show wSz
        let renderer = winRenderer win
            rm = resourceManager gui
            target = SDL.rendererRenderTarget renderer
            clip = SDL.rendererClipRect renderer
            go parentOff clipRect force3 widget = do
                w <- readMonadIORef widget
                let (SDL.Rectangle widgP widgSz) = widgetRect w
                    pInWinCoord = widgP .+^ parentOff
                    rect = rectIntersection clipRect $ SDL.Rectangle pInWinCoord widgSz
{-                liftIO $ putStrLn $ concat ["redrawWindow.go : parentOff=", show parentOff,
                    "  clipRect=", show clipRect, "  widgetRect=", show $ widgetRect w,
                    "  widgetCanvasRect=", show $ widgetCanvasRect w,
                    "  pInWinCoord=", show pInWinCoord, "   rect=", show rect] -}
                when ((widgetFlags w .&. WidgetVisible) /= WidgetNoFlags && not (isEmptyRect rect)) $ do
                        markedForRedraw <- isWidgetMarkedForRedrawing' w
                        let force4 = force3 || markedForRedraw
                            off = pInWinCoord .-. pointOfRect (widgetCanvasRect w)
                        when force4 $ do
                            when markedForRedraw $ clearWidgetRedrawFlag' w
                            clip $= Just (P.toSDLRect rect)
                            -- liftIO $ putStrLn $ concat ["redrawWindow clip rect=",show (P.toSDLRect rect)]
                            runCanvas renderer rm off $ -- do
                                (onDraw $ widgetFns w) widget
--                                visibleRect <- getVisibleRect widget
--                                setColor $ V4 255 0 0 0
--                                drawRect $ shrinkRect' 1 visibleRect

                        V.mapM_  (go off rect force4) (cildrenWidgets w)
--                liftIO $ putStrLn $ concat ["redrawWindow.go END "]
        buf <- if szChanged then do
                     SDL.destroyTexture $ winBuffer win
                     newBuf <- P.createTargetTexture renderer wSz
                     writeMonadIORef rfWin win{winBuffer=newBuf}
                     return newBuf
               else return $ winBuffer win
        target $= Just buf
        let winRect = SDL.Rectangle zero wSz
        go zero winRect force2 (mainWidget win)
        clip   $= Nothing
        target $= Nothing
        SDL.clear renderer
        SDL.copy renderer buf Nothing  Nothing -- $ Just $ winRect
        SDL.present renderer

isSpecStateWidget:: MonadIO m => GuiWindow -> Widget -> m Bool
isSpecStateWidget rfWin widget = do
    s <- specStateWidget <$> readMonadIORef rfWin
    case s of
      WidgetStateMouseCaptured t -> return $ widget == t
      _ -> return False

resetSpecStateWidget :: MonadIO m => GuiWindow -> m ()
resetSpecStateWidget win = modifyMonadIORef' win (\x -> x{specStateWidget=WidgetNoSpecState})
{-# INLINE resetSpecStateWidget #-}

setSpecStateWidget ::  MonadIO m => Widget -> SpecStateWidget -> m ()
setSpecStateWidget widget s = (`modifyMonadIORef'` (\x -> x{specStateWidget=s})) =<< getWidgetWindow widget
{-# INLINE setSpecStateWidget #-}

setMouseCapturedWidget ::  MonadIO m => Widget -> m ()
setMouseCapturedWidget widget = setSpecStateWidget widget $ WidgetStateMouseCaptured widget
{-# INLINE setMouseCapturedWidget #-}

getMouseCapturedWidget :: MonadIO m => GuiWindow -> m (Maybe Widget)
getMouseCapturedWidget rfWin = do
    s <- specStateWidget <$> readMonadIORef rfWin
    case s of
        WidgetStateMouseCaptured t -> return $ Just t
        _ -> return Nothing
{-# INLINE getMouseCapturedWidget #-}

resetMouseCaptured :: MonadIO m => GuiWindow -> m ()
resetMouseCaptured rfWin = do
    w <- readMonadIORef rfWin
    case specStateWidget w of
        WidgetStateMouseCaptured _ -> writeMonadIORef rfWin w{specStateWidget=WidgetNoSpecState}
        _ -> return ()
{-# INLINE resetMouseCaptured #-}

resetMouseCapturedWidget :: MonadIO m => Widget -> m ()
resetMouseCapturedWidget widget = getWidgetWindow widget >>= resetMouseCaptured
{-# INLINE resetMouseCapturedWidget #-}

setWinMainMenu :: MonadIO m => GuiWindow -> Maybe Widget -> m ()
setWinMainMenu rfWin menu = modifyMonadIORef' rfWin (\x -> x{winMainMenu=menu})
{-# INLINE setWinMainMenu #-}

getWinMainMenu :: MonadIO m => GuiWindow -> m (Maybe Widget)
getWinMainMenu = fmap winMainMenu . readMonadIORef
{-# INLINE getWinMainMenu #-}

setWinNext :: MonadIO m => GuiWindow -> Maybe GuiWindow -> m ()
setWinNext rfWin nextWindow = modifyMonadIORef' rfWin (\x -> x{winNext=nextWindow})
{-# INLINE setWinNext #-}

getWinNext :: MonadIO m => GuiWindow -> m (Maybe GuiWindow)
getWinNext = fmap winNext . readMonadIORef
{-# INLINE getWinNext #-}
