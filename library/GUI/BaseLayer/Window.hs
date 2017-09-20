{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:      GUI.BaseLayer.Window
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Функции связанные с окном GUI, выделенные в модуль, не зависящий от "GUI.BaseLayer.Widget"
-- для уменьшения зависимостей модулей.

module GUI.BaseLayer.Window(
     -- * Низкоуровневые функции доступа к полям записи окна.
     getSDLWindow,getWindowRenderer,getWindowGui,getWinIx',getWinIx,getWindowMainWidget
    ,getWindowForegroundWidget,getFocusedWidget,setFocusedWidget,getWidgetUnderCursor,setWidgetUnderCursor
    ,getWinCursorIx,setWinCursorIx,getWinMainMenu,setWinMainMenu,getWinRetcode,setWinRetcode,setWinOnClosed
    ,setWinOnCloseConfirm
    -- * Вызов обработчиков изменения состояния окна.
    ,doWinOnClosed,canWinClose
     -- * Извлечение SDL кода окна.
    ,getWinId'',getWinId',getWinId
    -- * Обращение к оконным функциям SDL
    ,getWindowAbsolutePosition,fromWinToScreenPoint,getFocusedWin
    -- * Флаги окна.
    ,pattern WindowNoFlags,pattern WindowRedrawFlag,pattern WindowCloseOnLostFocuse,pattern WindowWaitAlt
    ,pattern WindowPopupFlag,pattern WindowLocked,pattern WindowCloseOnEsc
    ,pattern WindowHaveKeyboardFocus, pattern WindowHaveMouseFocus,pattern WindowClickable
    ,pattern WindowWaitPopup,pattern WindowWaitPopupReset
    ,removeWindowFlags,getWindowFlags,setWindowFlags,windowFlagsAddRemove,windowFlagsAdd
    ,windowFlagsRemove,allWindowFlags',allWindowFlags,anyWindowFlags
    -- * Логирование и обработка ошибок.
    ,logPutLnWindow,logOnErrInWindow',logOnErrInWindow
    -- * Прочее.
    ,getSkinFromWin
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Bits
import qualified TextShow as TS
import qualified SDL
import qualified SDL.Raw as Raw
import qualified SDL.Internal.Types
import SDL.Vect
import GUI.BaseLayer.Depend0.Types
import GUI.BaseLayer.Depend0.Auxiliaries
import GUI.BaseLayer.Depend0.Ref
import GUI.BaseLayer.Depend0.BitFlags
import GUI.BaseLayer.Depend0.Cursor
import qualified GUI.BaseLayer.Depend1.Logging as L (logPutLn,logOnSomeException)
import GUI.BaseLayer.Depend1.Skin (Skin)
import GUI.BaseLayer.Types
import GUI.BaseLayer.GUIRecord
import qualified GUI.BaseLayer.Primitives as P

pattern WindowNoFlags :: WindowFlags
pattern WindowRedrawFlag :: WindowFlags
pattern WindowCloseOnLostFocuse :: WindowFlags
pattern WindowWaitAlt :: WindowFlags
pattern WindowPopupFlag :: WindowFlags
pattern WindowHaveKeyboardFocus :: WindowFlags
pattern WindowHaveMouseFocus :: WindowFlags
pattern WindowClickable :: WindowFlags
pattern WindowLocked :: WindowFlags
pattern WindowCloseOnEsc :: WindowFlags
pattern WindowWaitPopup :: WindowFlags
pattern WindowWaitPopupReset :: WindowFlags
                                      --  5432109876543210
pattern WindowNoFlags        =    Flags 0x0000000000000000
pattern WindowRedrawFlag =        Flags 0x0000000000000001
pattern WindowCloseOnLostFocuse = Flags 0x0000000000000002
pattern WindowWaitAlt =           Flags 0x0000000000000004
pattern WindowPopupFlag =         Flags 0x0000000000000008
pattern WindowHaveKeyboardFocus = Flags 0x0000000000000010
pattern WindowHaveMouseFocus =    Flags 0x0000000000000020
pattern WindowClickable =         Flags 0x0000000000000040
pattern WindowLocked =            Flags 0x0000000000000080
pattern WindowCloseOnEsc =        Flags 0x0000000000000100
pattern WindowWaitPopup  =        Flags 0x0000000000000200
pattern WindowWaitPopupReset =    Flags 0x0000000000000400

-- | Получить SDL тип окна из окна GUI.
getSDLWindow:: MonadIO m => Window -> m SDL.Window
getSDLWindow rfWin = winSDL <$> readMonadIORef rfWin
{-# INLINE getSDLWindow #-}

-- | Получить SDL рендерер (визуализатор) окна.
getWindowRenderer:: MonadIO m => Window -> m SDL.Renderer
getWindowRenderer rfWin = winRenderer <$> readMonadIORef rfWin
{-# INLINE getWindowRenderer #-}

-- | Получить ссылку 'Gui' на 'GUIRecord'. Все окна имеют ссылки на один и тот же 'GUIRecord'.
getWindowGui:: MonadIO m => Window -> m Gui
getWindowGui rfWin = guiOfWindow <$> readMonadIORef rfWin
{-# INLINE getWindowGui #-}

-- | Получить индекс окна по которому окно идентифицируется в SDL сообщениях и индексируется в GUI,
-- в коллекции окон.
-- Функция принимает запись окна 'WindowRecord', а не ссылку на эту запись 'Window'.
-- Для оптимизации, когда запись по ссылке уже получена.
getWinIx':: WindowRecord -> GuiWindowIx
getWinIx' = winSDL
{-# INLINE getWinIx' #-}

-- | Получить индекс окна по которому окно идентифицируется в SDL сообщениях и индексируется в GUI,
-- в коллекции окон.
getWinIx:: MonadIO m => Window -> m GuiWindowIx
getWinIx rfWin = getWinIx' <$> readMonadIORef rfWin
{-# INLINE getWinIx #-}

-- | Получить главный (корневой) виджет основного, нижнего слоя виджетов окна.
getWindowMainWidget :: MonadIO m => Window -> m Widget
getWindowMainWidget rfWin = mainWidget <$> readMonadIORef rfWin
{-# INLINE getWindowMainWidget #-}

-- | Получить главный (корневой) виджет дополнительного, верхнего слоя виджетов окна.
getWindowForegroundWidget :: MonadIO m => Window -> m Widget
getWindowForegroundWidget rfWin = winFgWidget <$> readMonadIORef rfWin
{-# INLINE getWindowForegroundWidget #-}

-- | Получить виджет в фокусе или Nothing.
getFocusedWidget :: MonadIO m => Window -> m (Maybe Widget)
getFocusedWidget = fmap focusedWidget . readMonadIORef
{-# INLINE getFocusedWidget #-}

-- | Установить виджет в фокусе (Just виджет) или сбросить фокус - Nothing.
-- Замечание: Это весьма низкоуровневая функция, которая только устанавливает соотвествующее поле в 'WindowRecord'.
-- Для сброса и установки фокуса в пользовательском коде используйте функции из "GUI.BaseLayer.Focus".
setFocusedWidget :: MonadIO m => Window -> Maybe Widget -> m ()
setFocusedWidget rfWin mb = modifyMonadIORef' rfWin (\x -> x{focusedWidget=mb})
{-# INLINE setFocusedWidget #-}

-- | Получить виджет который был ранее установлен как находящийся под курсором
-- (в его области отображения находился указатель мыши) или Nothing.
getWidgetUnderCursor :: MonadIO m => Window -> m (Maybe Widget)
getWidgetUnderCursor = fmap widgetUnderCursor . readMonadIORef
{-# INLINE getWidgetUnderCursor #-}

-- | Установить виджет на которым находится курсор.
-- Вызывается только из /GUI.BaseLayer/. Не вызывать из пользовательского кода.
setWidgetUnderCursor :: MonadIO m => Window -> Maybe Widget -> m ()
setWidgetUnderCursor rfWin mb = modifyMonadIORef' rfWin (\x -> x{widgetUnderCursor=mb})
{-# INLINE setWidgetUnderCursor #-}

-- | Возвращает индекс курсора установленного для окна.
getWinCursorIx :: MonadIO m => Window -> m CursorIx
getWinCursorIx = fmap curWinCursor . readMonadIORef
{-# INLINE getWinCursorIx #-}

-- | Установить индекс курсора для окна.
-- Вызывается только из /GUI.BaseLayer/. Не вызывать из пользовательского кода.
setWinCursorIx :: MonadIO m => Window -> CursorIx -> m ()
setWinCursorIx rfWin ix = modifyMonadIORef' rfWin (\x -> x{curWinCursor=ix})
{-# INLINE setWinCursorIx #-}

-- | Установить виджет как основное (горизонтальное) меню окна или сбросить - Nothing.
-- Устанавливается виджетом @GUI.Widget.Menu.Horizontal.horizontalMenu@.
-- Не следует использовать в иных случаях.
setWinMainMenu :: MonadIO m => Window -> Maybe Widget -> m ()
setWinMainMenu rfWin menu = modifyMonadIORef' rfWin (\x -> x{winMainMenu=menu})
{-# INLINE setWinMainMenu #-}

-- | Возвращает виджет, установленный как главное меню окна.
getWinMainMenu :: MonadIO m => Window -> m (Maybe Widget)
getWinMainMenu = fmap winMainMenu . readMonadIORef
{-# INLINE getWinMainMenu #-}

-- | Возвращает код возврата окна сообщаемый обработчику @winOnClosed@ после закрытия окна.
getWinRetcode :: MonadIO m => Window -> m WindowRetcode
getWinRetcode = fmap winRetcode . readMonadIORef
{-# INLINE getWinRetcode #-}

-- | Установить код возврата окна сообщаемый обработчику @winOnClosed@ после закрытия окна.
setWinRetcode :: MonadIO m => Window -> WindowRetcode -> m ()
setWinRetcode rfWin v = modifyMonadIORef' rfWin (\x -> x{winRetcode=v})
{-# INLINE setWinRetcode #-}

-- | Установить обработчик закрытия окна. (Поле @WindowRecord.winOnClosed@).
setWinOnClosed :: MonadIO m => Window -> (forall n. MonadIO n => WindowRetcode -> n ()) -> m ()
setWinOnClosed rfWin v = modifyMonadIORef' rfWin (\x -> x{winOnClosed=v})
{-# INLINE setWinOnClosed #-}

-- | Установить обработчик подтверждения закрытия окна. (Поле @WindowRecord.winCloseConfirm@).
setWinOnCloseConfirm :: MonadIO m => Window -> (forall n. MonadIO n => Window -> n Bool) -> m ()
setWinOnCloseConfirm rfWin v = modifyMonadIORef' rfWin (\x -> x{winCloseConfirm=v})
{-# INLINE setWinOnCloseConfirm #-}


---------------------- * Вызов обработчиков изменения состояния окна.

-- | Выполнить обработчика @winOnClosed@. Функция должна быть вызвана в __/GUI.BaseLayer.Core/__ .
doWinOnClosed :: MonadIO m => Window -> m ()
doWinOnClosed rfWin = do
    WindowRecord{..} <- readMonadIORef rfWin
    logOnErrInWindow rfWin "doWinOnClosed" $ winOnClosed winRetcode
{-# INLINE doWinOnClosed #-}

-- | Вызов обработчика @winCloseConfirm@ лдя подтверждения закрытия окна средствами ОС (нажание на [x] и подобные.
-- Разрешает закрыть окно если возвращает True
-- Функция должна быть вызвана в __/GUI.BaseLayer.Core/__ .
canWinClose :: MonadIO m => Window -> m Bool
canWinClose rfWin = do
    WindowRecord{..} <- readMonadIORef rfWin
    logOnErrInWindow' rfWin "canWinClosed" (return True) $ winCloseConfirm rfWin
{-# INLINE canWinClose #-}

----------------------------------------------------
-- No exported.
getSDLRawWindow':: SDL.Window -> Raw.Window
getSDLRawWindow' (SDL.Internal.Types.Window w) = w
{-# INLINE getSDLRawWindow' #-}

-- No exported.
getSDLRawWindow:: WindowRecord -> Raw.Window
getSDLRawWindow = getSDLRawWindow' . winSDL
{-# INLINE getSDLRawWindow #-}

-- | Получить код идентификатора окна из SDL окна. Код присваивается внутри SDL.
getWinId'':: MonadIO m => SDL.Window -> m GuiWindowId
getWinId'' = Raw.getWindowID . getSDLRawWindow'
{-# INLINE getWinId'' #-}

-- | Получить код идентификатора окна из 'WindowRecord'. Код присваивается внутри SDL.
getWinId':: MonadIO m => WindowRecord -> m GuiWindowId
getWinId' = Raw.getWindowID . getSDLRawWindow
{-# INLINE getWinId' #-}

-- | Получить код идентификатора окна из 'Window'. Код присваивается внутри SDL.
getWinId:: MonadIO m => Window -> m GuiWindowId
getWinId rfWin = getWinId' =<< readMonadIORef rfWin
{-# INLINE getWinId #-}

-----------------------------------------------------------------

-- | Возвращает абсолютную позицию левого верхнего угла окна.
getWindowAbsolutePosition :: MonadIO m => Window -> m GuiPoint
getWindowAbsolutePosition rfWin =
    (P . P.fromSDLV2) <$> (SDL.getWindowAbsolutePosition =<< getSDLWindow rfWin)
{-# INLINEABLE getWindowAbsolutePosition #-}

-- | Конвертирует точку из оконных координат  в экранные.
fromWinToScreenPoint :: MonadIO m => Window -> GuiPoint -> m GuiPoint
fromWinToScreenPoint rfWin (P p) = (.+^ p) <$> getWindowAbsolutePosition rfWin
{-# INLINEABLE fromWinToScreenPoint #-}

-- | Возвращает окно GUI приложения имеющее фокус.
getFocusedWin :: MonadIO m => Gui -> m (Maybe Window)
getFocusedWin gui = windowsFold gui f Nothing
    where f r@(Just _) _ = return r
          f _ win = do focused <- allWindowFlags win WindowHaveMouseFocus
                       return (if focused then Just win else Nothing)

-----------------------------------------------------------------
-- | Возвращает все флаги окна.
getWindowFlags:: MonadIO m => Window -> m WindowFlags
getWindowFlags = fmap winFlags .  readMonadIORef
{-# INLINE getWindowFlags #-}

-- | Устанавливает все флаги окна.  Вы должны знать что вы делаете!
setWindowFlags:: MonadIO m => Window -> WindowFlags -> m ()
setWindowFlags win fl = modifyMonadIORef' win (\x -> x{winFlags=fl})
{-# INLINE setWindowFlags #-}

-- | Добавляет флаг или несколько флагов объединённых по (.|.) к полю флагов окна.
windowFlagsAdd:: MonadIO m => Window -> WindowFlags -> m ()
windowFlagsAdd win add = modifyMonadIORef' win (\x -> x{winFlags=winFlags x .|. add})
{-# INLINE windowFlagsAdd #-}

-- | Удаляет флаг или несколько флагов объединённых по (.|.) из полю флагов окна.
windowFlagsRemove:: MonadIO m => Window -> WindowFlags -> m ()
windowFlagsRemove win rmv = modifyMonadIORef' win (\x -> x{winFlags=winFlags x .&. complement rmv})
{-# INLINE windowFlagsRemove #-}

-- | Чистая функция для удаления флагов из поля флагов окна.
removeWindowFlags:: WindowFlags -> WindowFlags -> WindowFlags
removeWindowFlags fl rmv = fl .&. complement rmv
{-# INLINE removeWindowFlags #-}

-- | Одновременно добавляет и удаляет флаги из полю флагов окна.
windowFlagsAddRemove:: MonadIO m => Window -> -- ^ ссылка на окно.
                                    WindowFlags -> -- ^ Добавляемые флаги.
                                    WindowFlags -> -- ^ удаляемые флаги.
                                    m ()
windowFlagsAddRemove win add rmv =
    modifyMonadIORef' win (\x -> x{winFlags=(winFlags x .&. complement rmv) .|. add})
{-# INLINE windowFlagsAddRemove #-}

-- | Проверяет, все ли из указанных флагов установлены.
-- Если задан только один флаг - эквивалентна проверки флага.
-- Функция принимает запись окна 'WindowRecord', а не ссылку на эту запись 'Window'.
-- Для оптимизации, когда запись по ссылке уже получена.
allWindowFlags':: WindowRecord -> WindowFlags -> Bool
allWindowFlags' w fl = fl == (fl .&. winFlags w)
{-# INLINE allWindowFlags' #-}

-- | Проверяет, все ли из указанных флагов установлены.
-- Если задан только один флаг - эквивалентна проверки флага.
allWindowFlags:: MonadIO m => Window -> WindowFlags -> m Bool
allWindowFlags win fl = (`allWindowFlags'` fl) <$> readMonadIORef win
{-# INLINE allWindowFlags #-}

-- | Возвращает True если любой из указанных флагов установлен.
-- Если задан только один флаг - эквивалентна проверки флага.
anyWindowFlags:: MonadIO m => Window -> WindowFlags -> m Bool
anyWindowFlags win fl = ((WindowNoFlags /=) . (fl .&.) . winFlags) <$> readMonadIORef win
{-# INLINE anyWindowFlags #-}

-------------------------------- * Логирование и обработка ошибок.

-- | Вывод сообщения в журнал.
logPutLnWindow :: MonadIO m => Window -> -- ^ Ссылка на GUI окно.
                               TS.Builder -> -- ^ TextShow Билдер из пакета __text-show__.
                               m ()
logPutLnWindow window t = (`L.logPutLn` t) =<< guiGetLog =<< getWindowGui window

-- | Выполнение некоторого действия в контексте окна, и, если, при этом возникнет исключение, вывести его в журнал.
logOnErrInWindow' :: MonadIO m => Window -> -- ^ Ссылка на GUI окно.
                                  TS.Builder -> -- ^ Префикс сообщения об исключении, если оно возникнет.
                                  IO a -> -- ^ Действие выполняемое в случае возникновение исключения.
                                  IO a -> -- ^ Действие в котором может возникнуть неперехваченное исключение.
                                  m a
logOnErrInWindow' window t h f = liftIO $ guiCatch f (\e -> do
        l <- guiGetLog =<< getWindowGui window
        L.logOnSomeException l t e >> h)
{-# INLINEABLE logOnErrInWindow' #-}

-- | Выполнение некоторого действия в контексте окна, и, если, при этом возникнет исключение, вывести его в журнал.
logOnErrInWindow :: MonadIO m => Window -> -- ^ Ссылка на GUI окно.
                                 TS.Builder -> -- ^ Префикс сообщения об исключении, если оно возникнет.
                                 IO () -> -- ^ Действие в котором может возникнуть неперехваченное исключение.
                                 m ()
logOnErrInWindow window t = logOnErrInWindow' window t (return ())
{-# INLINEABLE logOnErrInWindow #-}


-----------------------------------------------------------------

-- | Возвращает 'Skin' из окна.
getSkinFromWin:: MonadIO m => Window -> m Skin
getSkinFromWin = guiGetSkin <=< getWindowGui
{-# INLINE getSkinFromWin #-}

