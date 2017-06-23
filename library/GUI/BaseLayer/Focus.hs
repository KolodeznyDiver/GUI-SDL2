{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      GUI.BaseLayer.Focus
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <kolodeznydiver@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Поддержка виджета с фокусом. Функции этого модуля могут вызываться пользовательским кодом.
-- Установка фокуса для виджета означает что ему будут адресованы сообщения по вводе с клавиатуры
-- (обработчики сообщений базового уровня виджета @onKeyboard@ и @onTextInput@.
-- Так же, у виджета устанавливается флаг @WidgetFocused@, проверяя который он может изменить вид своего отображения.

module GUI.BaseLayer.Focus(
        clearFocusInWindow,clearWidgetFocus,setWidgetFocus

    ) where

import Control.Monad
import Control.Monad.IO.Class -- (MonadIO)
import Data.Bits
import Maybes (whenIsJust)
import GUI.BaseLayer.Depend0.Ref
import GUI.BaseLayer.Types
import GUI.BaseLayer.Widget

-- | Не экспотритруется.
-- Окну теряющему фокус посылается @onLostKeyboardFocus@.
clearWidgetFocusInternal :: MonadIO m => Widget -> m ()
clearWidgetFocusInternal widget = do
    widgetFlagsRemove widget WidgetFocused
    markWidgetForRedraw widget
    fns <- getWidgetFns widget
    logOnErrInWidget widget "clearWidgetFocusInternal.onLostKeyboardFocus" $
        onLostKeyboardFocus fns widget
{-# INLINE clearWidgetFocusInternal #-}

-- | Сбросить фокус в окне. Ни один виджет в окне ни будет в фокусе.
-- Виджету теряющему фокус посылается @onLostKeyboardFocus@.
clearFocusInWindow :: MonadIO m => Window -> m ()
clearFocusInWindow rfWin = do
    win <- readMonadIORef rfWin
    whenIsJust (focusedWidget win) $ \ widget -> do
        writeMonadIORef rfWin win{focusedWidget=Nothing}
        clearWidgetFocusInternal widget

-- | Сбросить фокус для указанного виджета если он был в фокусе.
-- Ни один виджет в окне ни будет в фокусе.
-- Виджету теряющему фокус посылается @onLostKeyboardFocus@.
clearWidgetFocus :: MonadIO m => Widget -> m ()
clearWidgetFocus widget = do
    focused <- allWidgetFlags widget WidgetFocused
    when focused (clearFocusInWindow =<< getWidgetWindow widget)
{-# INLINE clearWidgetFocus #-}

-- | Установить фокус для указанного виджета. Виджет должен быть видим - 'WidgetVisible',
-- разрешёе 'WidgetEnable', и предназначен для приёма фокуса - 'WidgetFocusable'.
-- Если другой вилдет был в фокусе, то с него снимается фокус.
-- Виджету теряющему фокус посылается @onLostKeyboardFocus@, после этого Виджету получающему
-- фокус посылается @onGainedKeyboardFocus@.
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
        logOnErrInWidget widget "setWidgetFocus.onGainedKeyboardFocus" $
            onGainedKeyboardFocus (widgetFns widg) widget

