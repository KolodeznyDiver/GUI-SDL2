{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}

-- |
-- Module:      GUI.Widget.Handlers
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Заготовки наборов - обработчиков событий, используемые в разных виджетах.
-- (Повторное использование кода рекомендуется лучшими ...)

module GUI.Widget.Handlers(
    noChildrenFns,colorRectFns,grayRectFns
    ,MouseAnimatedHndlr(..),MouseAnimatedClickableHndlr(..)
    ,noChildrenClickableHndlr,noChildrenMouseAnimatedHndlr
        ) where

import Control.Monad.IO.Class
import Control.Monad
import Data.IORef
import Data.Default
import qualified SDL
import GUI

-- | Набор обработчиков базового виджета, годится для большинства виджетов
-- не имеющих дочерних виджетов.
noChildrenFns :: GuiSize -> -- ^ Размеры виджета без полей.
                 WidgetFunctions
noChildrenFns initInsideSz = def{
    onCreate = \widget -> notifyParentAboutSize widget initInsideSz
    ,onResizing= \widget -> void . extendableOnResizing initInsideSz widget
                             }
{-# INLINEABLE noChildrenFns #-}

-- | Годится для демонстрации - отрисовки прямоугольника заданного размера и цвета.
colorRectFns :: GuiSize -> GuiColor -> WidgetFunctions
colorRectFns sz color = (noChildrenFns sz){
    onDraw= \widget -> setColor color >> getVisibleRect widget >>= fillRect
                                           }
-- | Годится для демонстрации - отрисовки прямоугольника заданного размера и цвета серой шкалы.
grayRectFns:: GuiSize -> ColorComponent -> WidgetFunctions
grayRectFns sz = colorRectFns sz . grayColor
{-# INLINE grayRectFns #-}

-- | Запись, которая пригодится для виджета отслеживающего и анимирующего своё положение
-- в зависимости от положения мыши. Она генерируется @noChildrenMouseAnimatedHndlr@.
data MouseAnimatedHndlr = MouseAnimatedHndlr {
      -- | Состояние мыши относительно виджета. См. "GUI.Widget.Types".
      mouseAnimatedMouseState :: IORef WidgetMouseState
      -- | Событие, которое отреагирует и на нажатие и на отпускание мыши.
    , mouseAnimatedOnClick :: forall m. MonadIO m => Widget -> Bool -> GuiPoint -> m ()
      -- | Функции - обработчики событий базового виджета которые предлагается использовать.
      -- Никто не мешает их далее модифицировать.
    , mouseAnimatedFs :: WidgetFunctions
        }

-- | Функция, создающая 'MouseAnimatedHndlr' из которого просто делается виджет без потомков.
-- Например, "GUI.Widget.TH.LinearTrackBar".
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

-- | Запись, которая пригодится для виджета отслеживающего и анимирующего своё положение
-- в зависимости от положения мыши и кнопки Enter, когда виджет в фокусе.
-- Она генерируется @noChildrenClickableHndlr@.
data MouseAnimatedClickableHndlr = MouseAnimatedClickableHndlr {
      -- | Состояние мыши относительно виджета. См. "GUI.Widget.Types".
      mouseAnimatedClickableMouseState :: IORef WidgetMouseState
      -- | Событие, которое отреагирует на нажатие мыши и кнопки Enter если виджет будет в фокусе.
    , mouseAnimatedClickableAction :: IORef NoArgAction
      -- | Функции - обработчики событий базового виджета которые предлагается использовать.
      -- Никто не мешает их далее модифицировать.
    , mouseAnimatedClickableFs :: WidgetFunctions
                                                               }

-- | Функция, создающая 'MouseAnimatedClickableHndlr' из которого просто делается виджет без потомков.
-- Например, @GUI.Widget.Button.button@.
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
                        let shiftCtrlAlt = getShftCtrlAlt km in
                        when (shiftCtrlAlt == ShiftCtrlAlt False False False &&
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


