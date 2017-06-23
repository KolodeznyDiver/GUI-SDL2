-- |
-- Module:      GUI.BaseLayer.SpecStateWidget
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <kolodeznydiver@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Поддержка специального виджета окна.
--
-- В окне имеется поле @specStateWidget :: SpecStateWidget@ , которое подразумевает что,
-- в один момент в окне может быть не более одного виджета с некиеми специальными свойствами.
--
-- В настоящий момент таким виджетом может быть только виджет захвативший мышь (Mouse Captured Widget).
-- При захвате мыши сообщение о выходе мыши за пределы виджета (но не окна) не посылается, сообщения
-- о движении мыши и нажатии её кнопок продолжают посылаться виджету захватившему управление мышью.
--
-- Захват и освобождение управления выполняется програмно в коде виджета с помощью
-- @setMouseCapturedWidget@, @resetMouseCapturedWidget@.

module GUI.BaseLayer.SpecStateWidget(
    -- * Обобщённый специальный виджет.
    isSpecStateWidget,resetSpecStateWidget,setSpecStateWidget
    -- * Mouse captured widget, как часный случай.
    ,setMouseCapturedWidget,getMouseCapturedWidget,resetMouseCaptured,resetMouseCapturedWidget
    ) where

import Control.Monad.IO.Class
import GUI.BaseLayer.Depend0.Ref
import GUI.BaseLayer.Types
import GUI.BaseLayer.Widget

-- | Указанный виджет установлен как специальный?
isSpecStateWidget:: MonadIO m => Window -> Widget -> m Bool
isSpecStateWidget rfWin widget = do
    s <- specStateWidget <$> readMonadIORef rfWin
    case s of
      WidgetStateMouseCaptured t -> return $ widget == t
      _ -> return False

-- | Сбросить специальный виджет у окна.
-- Для использования в пользователском коде предназначена @resetMouseCaptured@, а для
-- виджета (что, обычно, удобнее) @resetMouseCapturedWidget@.
resetSpecStateWidget :: MonadIO m => Window -> m ()
resetSpecStateWidget win = modifyMonadIORef' win (\x -> x{specStateWidget=WidgetNoSpecState})
{-# INLINE resetSpecStateWidget #-}

-- |
setSpecStateWidget ::  MonadIO m => Widget -> SpecStateWidget -> m ()
setSpecStateWidget widget s = (`modifyMonadIORef'` (\x -> x{specStateWidget=s})) =<< getWidgetWindow widget
{-# INLINE setSpecStateWidget #-}

-- | Установить указанный виджет как мouse captured widget.
setMouseCapturedWidget ::  MonadIO m => Widget -> m ()
setMouseCapturedWidget widget = setSpecStateWidget widget $ WidgetStateMouseCaptured widget
{-# INLINE setMouseCapturedWidget #-}

-- | Указанный виджет установлен как мouse captured widget?
getMouseCapturedWidget :: MonadIO m => Window -> m (Maybe Widget)
getMouseCapturedWidget rfWin = do
    s <- specStateWidget <$> readMonadIORef rfWin
    case s of
        WidgetStateMouseCaptured t -> return $ Just t
        _ -> return Nothing
{-# INLINE getMouseCapturedWidget #-}

-- | Сбросить мouse captured widget у окна.
resetMouseCaptured :: MonadIO m => Window -> m ()
resetMouseCaptured rfWin = do
    w <- readMonadIORef rfWin
    case specStateWidget w of
        WidgetStateMouseCaptured _ -> writeMonadIORef rfWin w{specStateWidget=WidgetNoSpecState}
        _ -> return ()
{-# INLINE resetMouseCaptured #-}

-- | Сбросить мouse captured widget у окна которому принадлежит указанный виджет.
resetMouseCapturedWidget :: MonadIO m => Widget -> m ()
resetMouseCapturedWidget widget = getWidgetWindow widget >>= resetMouseCaptured
{-# INLINE resetMouseCapturedWidget #-}


