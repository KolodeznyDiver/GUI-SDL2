{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      GUI.BaseLayer.Core
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <kolodeznydiver@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- В этом модуле собраны функции требующие множесто зависимостей от других модулей иерархии /GUI.BaseLayer/.

module GUI.BaseLayer.Core(
    -- * Создание окон.
    newWindow',newWindow,newModalWindow',newModalWindow
    -- * Композиция виджетов. Присоединение виджета к контейнеру для составления дерева виджетов.
    ,WidgetComposer(..),newForeground
    -- * Действия захватывающие все окна.
    ,forEachWidgetsInAllWin,notifyEachWidgetsInAllWin,setGuiState,getGuiState
    -- * Удаление окон.
    ,delWindowByIx,delWindow,delAllWindows,delWindowsBy,delAllPopupWindows
    -- * Удаление виджетов.
    ,delWidget,delAllChildWidgets
    -- * Перестановки дочерних виджетов.
    ,lastChildReplaceFirst
    -- * Наборы функций - обработчиков событий базового виджета.
    ,overlapsChildrenFns
    -- * Изменение окна.
    ,setWinSize',setWinSize
    -- * Proxy Canvas.
    ,runProxyWinCanvas,runProxyCanvas
                 ) where

import Control.Monad
import Control.Monad.IO.Class -- (MonadIO)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Data.Default
import Maybes (whenIsJust)
import Data.StateVar
import qualified SDL
import SDL.Vect
import GUI.BaseLayer.Depend0.Types
import GUI.BaseLayer.Depend0.Auxiliaries
import GUI.BaseLayer.Depend0.Cursor
import GUI.BaseLayer.Depend0.Ref
import GUI.BaseLayer.Depend1.Geometry
import GUI.BaseLayer.Depend1.Skin
import GUI.BaseLayer.Types
import GUI.BaseLayer.Window
import GUI.BaseLayer.Widget
import qualified GUI.BaseLayer.Primitives as P
import Data.Vector.Utils
import GUI.BaseLayer.Canvas
import GUI.BaseLayer.Action
import System.Utils (withRemoveFromTaskbar)
import GUI.BaseLayer.SpecStateWidget
import GUI.BaseLayer.GUIRecord

----------- * Создание окон.

-- | Создание обыкновенного окна (в том числе и popup).
-- Первое созданное окно должнобыть главным окном приложения - оно останется в панели задач.
-- Последующие создаваемые окна удаляются из панели задач.
newWindow':: MonadIO m =>
    Gui -> -- ^ Ссылка на 'GUIRecord' - единую для всего GUI запись.
    T.Text -> -- ^ Заголовок окна.
    WindowFlags -> -- ^ GUI флаги окна, см. "GUI.BaseLayer.Window".
    SDL.WindowConfig -> -- ^ параметры создания SDL окна.
    m Window
newWindow' rfGui winTitle winFl winCfg = do
    isMainWin <- Map.null <$> getWindowsMap rfGui
    wSDL <- if isMainWin then SDL.createWindow winTitle winCfg
            else do
                tmpTitle <- mkRandomWinTitle
                w <- withRemoveFromTaskbar (T.unpack tmpTitle) $ SDL.createWindow tmpTitle winCfg
                SDL.windowTitle w $= winTitle
                return w
    rSDL  <- SDL.createRenderer wSDL (-1) SDL.defaultRenderer
    sz <- P.fromSDLV2 <$> get (SDL.windowSize wSDL)
    buf <- P.createTargetTexture rSDL sz
    proxyTexture <- P.createTargetTexture rSDL $ V2 1 1
    fgTexture <- P.createTargetTexture rSDL $ V2 1 1
    textureCache <- newMonadIORef HM.empty
    rfWin <- newMonadIORef WindowRecord  { guiOfWindow = rfGui
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
                                   , winMainMenu = Nothing
                                   , winTextureCache = textureCache
                                   , winFgWidget = undefined
                                   , winFgBuffer = fgTexture
                                   , winRetcode = WindowRetcode 0
                                   , winCloseConfirm = \_ -> return True
                                   , winOnClosed = \_ -> return ()

                                   }
    let rect = SDL.Rectangle zero sz
        foregroundFns = def{
            onSizeChangedParentNotify= \_widget child size -> do
                p <- getRectLT <$> getWidgetRect child
                widgetResizingIfChanged child (SDL.Rectangle p size)
                                   }
        mkRoot fns = do
            rfW <- mkWidget' rfWin WidgetVisible WidgetMarginNone undefined fns
            setWidgetParent rfW rfW
            setWidgetRect rfW rect
            setWidgetCanvasRect  rfW rect
            return rfW
    mainWdgt <- mkRoot $ overlapsChildrenFns zero
    fgWdgt <- mkRoot foregroundFns
    modifyMonadIORef' rfWin (\x -> x{mainWidget=mainWdgt, winFgWidget=fgWdgt})
    guiUpdateWindows rfGui (Map.insert wSDL rfWin)
{-    sDbg <- showWindowsAsStr rfGui
    liftIO $ putStrLn $ concat ["newWindow' new = ",show wSDL, "   ",sDbg] -}
    return rfWin

-- | Создание обыкновенного окна. Подробнее см. @newWindow'@.
-- Отличие этой функции только в отсутствии парметра типа 'WindowFlags'.
-- Принимается что будет установлен только флаг @WindowRedrawFlag@ (обязательный для видимого окна).
newWindow:: MonadIO m => Gui -> -- ^ Ссылка на 'GUIRecord' - единую для всего GUI запись.
                         T.Text -> -- ^ Заголовок окна.
                         SDL.WindowConfig -> -- ^ параметры создания SDL окна.
                         m Window
newWindow rfGui winTitle = newWindow' rfGui winTitle WindowRedrawFlag
{-# INLINE newWindow #-}

-- | Создание модального окна. Не должно быть первым.
-- Пока окно открыто, другие окна недоступны. При создании второго модального окна они логически
-- располагаются стекомю. Т.е. при закрытии последнего модального предпоследнее становится доступным.
-- В остальном, см. @newWindow'@.
newModalWindow':: MonadIO m => Gui -> T.Text -> WindowFlags -> SDL.WindowConfig -> m Window
newModalWindow' rfGui winTitle winFl winCfg = do
    allWindowsMap_ (`windowFlagsAdd` WindowLocked) rfGui
    nw <- newWindow' rfGui winTitle winFl winCfg
    nWinIx <- getWinIx nw
    modifyMonadIORef' rfGui (\x -> x{guiModalWins=nWinIx:guiModalWins x})
    return nw

-- | Создание модального окна. Подробнее см. @newModalWindow'@.
-- Отличие этой функции только в отсутствии парметра типа 'WindowFlags'.
-- Принимается что будет установлен только флаг @WindowRedrawFlag@ (обязательный для видимого окна).
newModalWindow:: MonadIO m => Gui -> T.Text -> SDL.WindowConfig -> m Window
newModalWindow rfGui winTitle = newModalWindow' rfGui winTitle WindowRedrawFlag
{-# INLINE newModalWindow #-}


----------- * Композиция виджетов. Присоединение виджета к контейнеру для составления дерева виджетов.

infixr 0 $+

-- | Экземплярами класса 'WidgetComposer' являются контейнеры виджетов в которые можно вставить виджет.
class WidgetComposer a where
    ($+) :: MonadIO m => a -> (Widget -> Skin -> m (GuiWidget b)) -> m (GuiWidget b)

-- | 'Window' является контейнером виджетов. Функция ($+) вставляет виджет в осносное дерево виджетов окна.
-- и, хотя, у вставленного виджета будет родитель, специальный корневой виджет, он будет главным
-- отображаемым виджетом окна.
-- Попытка вставить следующий виджет в окно приведёт у удалению предыдущего.
instance WidgetComposer Window where
    rfWin $+ initF = do
             mainW <- getWindowMainWidget rfWin
             delAllChildWidgets mainW
             createWidget mainW initF

-- | Функция newForeground подобна ($+), но вставляет виджет в Foreground дерево виджетов окна.
-- Таки образом вставляются всплывающие подсказки, выпадающие списки, элементы в процессе перетаскивания,
-- всё то что лежит на верхнем (foreground) слое над остальными виджетами.
-- Попытка вставить следующий виджет в окно приведёт у удалению предыдущего.
newForeground :: MonadIO m => Window -> GuiPoint ->
                    (Widget -> Skin -> m (GuiWidget b)) -> m (GuiWidget b)
newForeground rfWin p initF = do
    fgWidget <- getWindowForegroundWidget rfWin
    delAllChildWidgets fgWidget
    gw <- createWidget fgWidget initF
    let widget = getWidget gw
    modifyMonadIORef' widget $ \ w ->
        w{widgetRect=SDL.Rectangle p $ sizeOfRect $ widgetRect w, widgetMargin = MarginLTRB 0 0 0 0}
    return gw

------------------------ * Действия захватывающие все окна.

-- | Выполнить действие для каждого виджета во всех окнах.
forEachWidgetsInAllWin :: MonadIO m => Gui -> (Widget -> m ()) -> m ()
forEachWidgetsInAllWin gui f = allWindowsMap_ ( (`forEachWidgets` f) <=< getWindowMainWidget) gui
{-# INLINE forEachWidgetsInAllWin #-}

-- | Для каждого виджета во всех окнах вызвать @onNotify@ с указанным кодом, и, возможно, исходным виджетом.
-- Предполагается использование в пользовательском коде.
notifyEachWidgetsInAllWin :: MonadIO m => Gui -> GuiNotifyCode -> Maybe Widget -> m ()
notifyEachWidgetsInAllWin gui nc mbW =
    forEachWidgetsInAllWin gui $ \widget -> do
        fns <- getWidgetFns widget
        logOnErr gui "notifyEachWidgetsInAllWin.onNotify" $ onNotify fns widget nc mbW
{-# INLINE notifyEachWidgetsInAllWin #-}

-- | Изменить код состояния программы и информировать об этом все виджеты.
-- Предполагается использование в пользовательском коде.
setGuiState :: MonadIO m => Gui -> GuiState -> m ()
setGuiState gui n = do
    g@GUIRecord{..} <- readMonadIORef gui
    when (guiState /= n) $ do
        writeMonadIORef gui g{guiState=n}
        forEachWidgetsInAllWin gui $ \widget -> do
            fns <- getWidgetFns widget
            logOnErr gui "setGuiState.onGuiStateChange" $ onGuiStateChange fns widget n
{-# INLINE setGuiState #-}

-- | Озвращает текущий код состояния программы. Функция находится здесь просто потому, что
-- рядом с @setGuiState@.
getGuiState :: MonadIO m => Gui -> m GuiState
getGuiState = fmap guiState . readMonadIORef
{-# INLINE getGuiState #-}

------------------------ * Удаление окон.

-- | Удаление окна по индексу. Именно она выполняет собственно удаление окна.
-- Остальные вызывают её.
delWindowByIx:: MonadIO m => Gui -> GuiWindowIx -> m ()
delWindowByIx gui winIx = do
--    cWins' <- getWindowsCount gui
    m <- getWindowsMap gui
{-
    g <- readMonadIORef gui
    modals <- case guiModalWins g of
                (modalIx:rest) | modalIx == winIx -> do
                    case rest of
                        [] -> mapM_ (`windowFlagsRemove` WindowLocked) $ Map.elems m
                        (prev:_) -> whenIsJust (Map.lookup prev m) (`windowFlagsRemove` WindowLocked)
                    return rest
                x -> return x  -}
    whenIsJust (Map.lookup winIx m) $ \ rfWin -> do
            delWidget =<< getWindowForegroundWidget rfWin
            delWidget =<< getWindowMainWidget rfWin
            win <- readMonadIORef rfWin
            SDL.destroyTexture $ winProxyTexture win
            SDL.destroyTexture $ winBuffer win
            SDL.destroyTexture $ winFgBuffer win
            SDL.destroyRenderer $ winRenderer win
            SDL.destroyWindow $ winSDL win
            mapM_ SDL.destroyTexture =<< readMonadIORef (winTextureCache win)
            guiUpdateWindowsAndModalWins gui updtFn
            doWinOnClosed rfWin
{-
            modifyMonadIORef' gui (\x -> x{guiWindows = Map.delete winIx $ guiWindows x
                                          ,guiModalWins = modals}) -}
{-            cWins <- getWindowsCount gui
            sDbg <- showWindowsAsStr gui
            liftIO $ putStrLn $ concat ["delWindowByIx cWins before = ",show cWins',
                 "   cWins after = ", show cWins, "   deleted = ",show $ winSDL win, "      ",sDbg] -}
  where updtFn wins mwins = let wins' = Map.delete winIx wins in do
                            modals <- case mwins of
                                        (modalIx:rest) | modalIx == winIx -> do
                                            case rest of
                                                [] -> mapM_ (`windowFlagsRemove` WindowLocked) $ Map.elems wins'
                                                (prev:_) -> whenIsJust (Map.lookup prev wins') (`windowFlagsRemove` WindowLocked)
                                            return rest
                                        x -> return x
                            return (wins', modals)

-- | Удаление окна по ссылке на GUI окно.
delWindow:: MonadIO m => Window -> m ()
delWindow rfWin = do
    win <- readMonadIORef rfWin
    delWindowByIx (guiOfWindow win) $ getWinIx' win

-- | Удаление всех окон.
delAllWindows:: MonadIO m => Gui -> m ()
delAllWindows gui = do
    m <- getWindowsMap gui
    case Map.keys m of
        [] -> return ()
        ks -> forM_ ks (delWindowByIx gui) >> delAllWindows gui

-- | Удаление окон удовлетворяющих предикату.
delWindowsBy:: MonadIO m => Gui -> (Window -> m Bool) -> m ()
delWindowsBy gui predicate = do
    m <- getWindowsMap gui
    forM_ (Map.toList m) $ \ (winIx,win) -> do
        b <- predicate win
        when b $ delWindowByIx gui winIx

-- | Удаление popup окон (всплывающих меню).
delAllPopupWindows :: MonadIO m => Gui -> m ()
delAllPopupWindows gui = delWindowsBy gui (`allWindowFlags` WindowPopupFlag)
{-# INLINE delAllPopupWindows #-}

-------------------------------- * Удаление виджетов.

-- | Удаление дерева виджетов начиная с указанного виджета.
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

-- | Удаление потомков указанного виджета рекрсивно, но не удаляя указанный виджет.
delAllChildWidgets  :: MonadIO m => Widget -> m ()
delAllChildWidgets widget = do
    w <- readMonadIORef widget
    V.mapM_ delWidget $ cildrenWidgets w
    modifyMonadIORef' widget (\x -> x{cildrenWidgets=V.empty})

----------------------------------- * Перестановки дочерних виджетов.

-- | Удаляет 0-ой виджет в векторе виджетов и на его место ставит последний.
lastChildReplaceFirst :: MonadIO m => Widget -> m ()
lastChildReplaceFirst widget = do
    delW <- (V.head . cildrenWidgets) <$> readMonadIORef widget
--    liftIO $ putStrLn "lastChildReplaceFirst"
    delWidget delW
    modifyMonadIORef' widget (\x -> x{cildrenWidgets= moveLastToFirst $ cildrenWidgets x})


------------------------------------ * Наборы функций - обработчиков событий базового виджета.
-- | Набор обработчиков сообщений для виджета у которого все (возможно один) потомок имеют
-- тот же размер и местоположение как родительский без полей, только у потомков это размер с полями.
overlapsChildrenFns :: GuiSize -> WidgetFunctions
overlapsChildrenFns initInsideSz = def{
    onCreate = (`notifyParentAboutSize` initInsideSz)
    ,onSizeChangedParentNotify= \widget child _ -> getWidgetCanvasRect widget >>= widgetResizingIfChanged child
    ,onResizing= \widget newRect -> do
        r <- extendableOnResizing initInsideSz widget newRect
        mapByWidgetChildren_ (\c -> do
                fs <- getWidgetFns c
                logOnErrInWidget widget "overlapsChildrenFns.onResizing" $ onResizing fs c r
                             ) widget
             }

-------------------------------- * Изменение окна.

-- | Устанавливает размер корневого (главного) виджета окна по заданному и вызывает для него @onResizing@.
-- Размеры совственно SDL окна (окна ОС) не меняются.
setWinSize' :: MonadIO m => Window -> GuiSize -> m ()
setWinSize' rfWin newSz = do
    widget <- getWindowMainWidget rfWin
    oldSz <- sizeOfRect <$> getWidgetRect widget
    when (newSz /= oldSz) $ do
        fs <- getWidgetFns widget
        let r = SDL.Rectangle zero newSz
        fgWidget <- getWindowForegroundWidget rfWin
        modifyMonadIORef' fgWidget $ \x -> x{widgetRect=r,widgetCanvasRect=r}
        logOnErrInWindow rfWin "setWinSize'.onResizing" $ onResizing fs widget r
        markWidgetForRedraw widget

-- | Устанавливает размеры SDL окна (окна ОС).
setWinSize :: MonadIO m => Window -> GuiSize -> m ()
setWinSize rfWin newSz = do
    winSDL <- getSDLWindow rfWin
    SDL.windowSize winSDL $= P.toSDLV2 newSz
    setWinSize' rfWin newSz
{-# INLINE setWinSize #-}

-------------------------------- * Proxy Canvas.

-- | Создать фиктивный Canvas для окна и выполнить в нём действие.
--  Используется для создания текстур и/или загрузки шрифтов вне @onDraw@.
-- Рисовать для непосредственного вывода на экран в proxy Canvas, и, в целом, не из @onDraw@ нельзя.
runProxyWinCanvas :: MonadIO m => Window -> Canvas m a -> m a
runProxyWinCanvas rfWin f = do
    win <- readMonadIORef rfWin
    rm  <- guiGetResourceManager $ guiOfWindow win
    P.withRendererTarget (winRenderer win) (winProxyTexture win) $
        runCanvas (winRenderer win) rm (winTextureCache win) zero f

-- | Создать фиктивный Canvas указывая виджет и выполнить в нём действие.
-- Cм. @runProxyWinCanvas@.
runProxyCanvas :: MonadIO m => Widget -> Canvas m a -> m a
runProxyCanvas widget f = getWidgetWindow widget >>= (`runProxyWinCanvas` f)
{-# INLINE runProxyCanvas #-}

