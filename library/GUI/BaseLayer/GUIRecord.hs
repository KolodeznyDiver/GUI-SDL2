{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module:      GUI.BaseLayer.GUIRecord
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <kolodeznydiver@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Функции, непосредственно использующие поля записи 'GUIRecord', обычно через ссылку на неё 'Gui'.
--
-- Модуль должен иметь небольшое число зависимостей. В частноти не зависеть от
-- "GUI.BaseLayer.Widget", "GUI.BaseLayer.Window", "GUI.BaseLayer.Core", для исключения циклических
-- зависимостей.

module GUI.BaseLayer.GUIRecord(
     -- * Функции, просто возвращающие значения полей.
     guiGetLog,guiGetSkin,getWindowsMap,guiGetResourceManager,guiGetActionsAndState
     -- * Функции использующие вектор окон @guiWindows :: GuiWindowCollection@ .
    ,getWindowsCount,getWindowByIx,doForWinByIx,allWindowsMap_
    ,windowsFold
    -- ** Отладочные.
    ,showWindowsAsStr
    -- * Функции модифицирующие вектор окон.
    ,guiUpdateWindowsAndModalWins,guiUpdateWindows
    -- * Логирования и обработки исключений.
    ,logPutLn,guiOnSomeException,logOnErr
    -- * Завершение выполнения GUI приложения.
    ,guiApplicationExitSuccess,guiApplicationExitFailure,guiApplicationExitWithCode
    -- * Прочие.
    ,guiUpdateActions,guiSetCursor
    ) where

import Control.Monad
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Exception
import System.Exit
import qualified Data.Map.Strict as Map
import Maybes (whenIsJust)
import qualified TextShow as TS
import GUI.BaseLayer.Depend0.Auxiliaries
import GUI.BaseLayer.Depend0.Cursor (CursorIx)
import GUI.BaseLayer.Depend0.Types
import GUI.BaseLayer.Depend0.Ref
import GUI.BaseLayer.Depend1.Action
import qualified GUI.BaseLayer.Depend1.Logging as L (logPutLn,logOnSomeException)
import GUI.BaseLayer.Depend1.Skin
import GUI.BaseLayer.Resource
import GUI.BaseLayer.Types
import GUI.BaseLayer.Depend1.Logging (GUILog)

-- | Возвращает активный журнал GUI приложения.
guiGetLog :: MonadIO m => Gui -> m GUILog
guiGetLog = fmap guiLog . readMonadIORef
{-# INLINE guiGetLog #-}

-- | Возвращает текущий 'Skin' GUI приложения.
guiGetSkin :: MonadIO m => Gui -> m Skin
guiGetSkin gui = guiSkin <$> readMonadIORef gui
{-# INLINE guiGetSkin #-}

-- | Возвращает @guiWindows@ - словарь всех окон индексированный по 'GuiWindowIx'.
getWindowsMap :: MonadIO m => Gui -> m GuiWindowCollection
getWindowsMap gui = guiWindows <$> readMonadIORef gui
{-# INLINE getWindowsMap #-}

-- | Возвращает менеджер ресурсов. Может потребоваться в пользовательском коде в редких случаях,
-- например, для удаления ресурса из кеша или добавления "вручную" сделанного курсора.
guiGetResourceManager :: MonadIO m => Gui -> m ResourceManager
guiGetResourceManager gui = resourceManager <$> readMonadIORef gui
{-# INLINE guiGetResourceManager #-}

-- | Возвращает значение двух полей : (@guiActions@,@guiState@)
guiGetActionsAndState :: MonadIO m =>
                         Gui ->
                         m (Actions,GuiState)
guiGetActionsAndState gui= do
    GUIRecord{..} <- readMonadIORef gui
    return (guiActions,guiState)
{-# INLINE guiGetActionsAndState #-}

-- | Возвращает текущее число окон GUI.
getWindowsCount :: MonadIO m => Gui -> m Int
getWindowsCount gui = Map.size <$> getWindowsMap gui
{-# INLINE getWindowsCount #-}

-- | Возвратить окно по индексу.
getWindowByIx :: MonadIO m => Gui -> GuiWindowIx -> m (Maybe Window)
getWindowByIx gui ix = (Map.lookup ix . guiWindows) <$> readMonadIORef gui
{-# INLINE getWindowByIx #-}

-- | Выполнить действие для окна по индексу, если такое есть.
doForWinByIx:: MonadIO m => (Window -> m ()) -> Gui -> GuiWindowIx -> m ()
doForWinByIx f gui winIx = (`whenIsJust` f) =<< Map.lookup winIx <$> getWindowsMap gui

-- | Выполнить действие для всех зарегестрированных в GUI окон.
allWindowsMap_:: MonadIO m => (Window -> m ()) -> Gui -> m ()
allWindowsMap_ f gui = mapM_ f =<< getWindowsMap gui
{-# INLINE allWindowsMap_ #-}

-- | Свёртка по окнам GUI.
windowsFold:: MonadIO m => Gui -> (a -> Window -> m a) -> a -> m a
windowsFold gui f a = getWindowsMap gui >>= foldM f a . Map.elems
{-# INLINE windowsFold #-}

-- | Вывод отладочного представления окон GUI в виде 'String'.
showWindowsAsStr :: MonadIO m => Gui -> m String
showWindowsAsStr gui = (show . Map.keys) <$> getWindowsMap gui

-- | Позволяет одновременно модифицировать поля @guiWindows@ и @guiModalWins@ пользовательской функцией.
guiUpdateWindowsAndModalWins :: MonadIO m => Gui ->
   (forall n. MonadIO n => GuiWindowCollection -> [GuiWindowIx] -> n (GuiWindowCollection,[GuiWindowIx])) ->
   m ()
guiUpdateWindowsAndModalWins gui f = do
    x <- readMonadIORef gui
    (nw,nm) <- f (guiWindows x) (guiModalWins x)
    writeMonadIORef gui x{guiWindows=nw, guiModalWins=nm}

-- | Позволяет модифицировать поле @guiWindows@ чистой пользовательской функцией.
guiUpdateWindows :: MonadIO m =>
   Gui ->
   (GuiWindowCollection -> GuiWindowCollection) ->
   m ()
guiUpdateWindows gui f = modifyMonadIORef' gui (\x -> x{guiWindows=f $ guiWindows x})
{-# INLINE guiUpdateWindows #-}

--------- * Логирования и обработки исключений.

-- | Вывод сообщения в журнал.
logPutLn :: MonadIO m => Gui -> TS.Builder -> m ()
logPutLn gui msg = (`L.logPutLn` msg) =<< guiGetLog gui

-- | Вывод информации об исключении. Функция должна вызываться из обработчика исключения.
guiOnSomeException :: Gui -> -- ^ Ссылка на 'GUIRecord'.
                      TS.Builder -> -- ^ Префикс сообщения об ошибке.
                      SomeException -> -- ^ Не обработанное в пользовательском коде исключение.
                      IO ()
guiOnSomeException gui t e = do
    l <- guiGetLog gui
    L.logOnSomeException l t e
{-# INLINE guiOnSomeException #-}

-- | Выполнение некоторого действия, и, если, при этом возникнет исключение, вывести его в журнал.
logOnErr :: MonadIO m => Gui -> -- ^ Ссылка на 'GUIRecord'.
                         TS.Builder -> -- ^ Префикс сообщения об ошибке.
                         IO () -> -- ^ Функция, которая может выбросить исключение.
                         m ()
logOnErr gui t f = liftIO $ guiCatch f (\e -> do
        l <- guiGetLog gui
        L.logOnSomeException l t e)
{-# INLINEABLE logOnErr #-}

--------- * Завершение выполнения GUI приложения.
--
-- В настоящий момент аргументы этих функций не используются и зарезервированы.

-- | Успешно завершить GUI приложение.
guiApplicationExitSuccess :: MonadIO m => Gui -> m ()
guiApplicationExitSuccess _gui = liftIO exitSuccess
{-# INLINE guiApplicationExitSuccess #-}

-- | Неуспешно завершить GUI приложение.
guiApplicationExitFailure :: MonadIO m => Gui -> m ()
guiApplicationExitFailure _gui = liftIO exitFailure
{-# INLINE guiApplicationExitFailure #-}

-- | Завершить GUI приложение с указанным кодом возврата.
guiApplicationExitWithCode :: MonadIO m => Gui -> Int -> m ()
guiApplicationExitWithCode gui 0 = guiApplicationExitSuccess gui
guiApplicationExitWithCode _gui code = liftIO $ exitWith $ ExitFailure code
{-# INLINE guiApplicationExitWithCode #-}

--------- * Прочие.

-- | Позволяет модифицировать поле @guiActions@ чистой пользовательской функцией.
guiUpdateActions :: MonadIO m =>
   Gui ->
   (Actions -> Actions) ->
   m ()
guiUpdateActions gui f = modifyMonadIORef' gui (\x -> x{guiActions=f $ guiActions x})
{-# INLINE guiUpdateActions #-}

-- | Установить указанный по индексу курсор на экране из кеша менеджера ресурсов.
guiSetCursor :: MonadIO m => Gui -> CursorIx -> m ()
guiSetCursor gui ix = (`rmSetCursor` ix) =<< guiGetResourceManager gui
{-# INLINE guiSetCursor #-}

