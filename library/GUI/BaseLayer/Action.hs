{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:      GUI.BaseLayer.Action
-- Copyright:   (c) 2017-2018 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Этот модуль, совместно с "GUI.BaseLayer.Depend1.Action", реализует поддержку так называемых Action's -
-- Действий в GUI. Код разделён на два модуля для уменьшения числа зависимостей между модулями.
--
-- Полробнее об Action-ах см. "GUI.BaseLayer.Depend1.Action".

module GUI.BaseLayer.Action(
    -- GUI.BaseLayer.Action.Internal.Action
    pattern AllInvisibleActionMask,pattern AllDisableActionMask,pattern AllEnableActionMask
    ,ActionMask(..),GuiState(..),ActionValue(..),Action(..),Actions
    ,ActionState(..),ActionFn(..)
    ,fromActionMask,getActionValueState,isVisibleActionState
    ,mkActionMask,mkEmptyActions
    -- GUI.BaseLayer.Action
    ,addActions,chkHotKey,getActionByGroupAndId,getActionsByGroupAndId,setActionEnable
    ,setAction,getVisibleActions

    -- * Функции, упрощающие назначение горячих клавиш при описании действий в пользовательском коде.
    ,kNoModifier,kCtrl,kShift,kAlt,kCtrlShift,kCtrlAlt,kShiftAlt
    ) where

import Data.Monoid
import Control.Monad.IO.Class -- (MonadIO)
import Data.Maybe
import qualified Data.Vector as V
import           Data.ByteString.Char8   (ByteString)
import           TextShow (showb)
import qualified SDL
import GUI.BaseLayer.Depend0.Ref
import GUI.BaseLayer.Depend0.Keyboard
--import GUI.BaseLayer.Depend1.Logging
import GUI.BaseLayer.Depend1.Action
import GUI.BaseLayer.Types
import GUI.BaseLayer.GUIRecord

kNoModifier :: SDL.Keycode -> Maybe KeyWithModifiers
kNoModifier = Just . KeyWithModifiers KeyNoModifiers
{-# INLINE kNoModifier #-}

kCtrl :: SDL.Keycode -> Maybe KeyWithModifiers
kCtrl = Just . KeyWithModifiers KeyCtrl
{-# INLINE kCtrl #-}

kShift :: SDL.Keycode -> Maybe KeyWithModifiers
kShift = Just . KeyWithModifiers KeyShift
{-# INLINE kShift #-}

kAlt :: SDL.Keycode -> Maybe KeyWithModifiers
kAlt = Just . KeyWithModifiers KeyAlt
{-# INLINE kAlt #-}

kCtrlShift :: SDL.Keycode -> Maybe KeyWithModifiers
kCtrlShift = Just . KeyWithModifiers KeyCtrlShift
{-# INLINE kCtrlShift #-}

kCtrlAlt :: SDL.Keycode -> Maybe KeyWithModifiers
kCtrlAlt = Just . KeyWithModifiers KeyCtrlAlt
{-# INLINE kCtrlAlt #-}

kShiftAlt :: SDL.Keycode -> Maybe KeyWithModifiers
kShiftAlt = Just . KeyWithModifiers KeyShiftAlt
{-# INLINE kShiftAlt #-}

-- | Добавить, дополнить или заменить действия в одной группе. Если группы нет, она создаётся.
addActions :: MonadIO m => Gui -> -- ^ Ссылка на GUI приложения.
                           ByteString -> -- ^ Имя группы.
                           [(ByteString,Action)] -> -- ^ Список новых действий с их строковыми идентификаторами.
                           m ()
addActions gui groupName lst = guiUpdateActions gui (actionAdd groupName lst)
{-# INLINEABLE addActions #-}

-- | Функция вызывается из @GUI.BaseLayer.RunGUI.onEvent@, выполняет действие соответствующее горячей клавише,
-- если для данного сочетания клавиш действие назначено и возвращает True если горячая клавиша найдена.
-- Не предназначена для использования вне GUI.BaseLayer.
chkHotKey :: MonadIO m => Gui -> KeyModifiers -> SDL.Keycode -> m Bool
chkHotKey gui km key = do
    (actions',state') <- guiGetActionsAndState gui
    let k = KeyWithModifiers km key
    case actionFindByHotKey state' actions' k of
        Just a -> logOnErr gui ("HotKey " <> showb k) (actionFn a) >> return True
        _ -> return False
{-# INLINE chkHotKey #-}

-- | Найти действие по имени группы и идентификатору в группе.
-- Возвращаются только видимые в настоящий момент действия (в зависимости от кода состояния программы).
getActionByGroupAndId  :: MonadIO m => Gui -> -- ^ Ссылка на GUI приложения.
                                       ByteString -> -- ^ Имя группы.
                                       ByteString -> -- ^ Идентификатор действия в группе.
                                       m (Maybe Action)
getActionByGroupAndId gui groupName idName = do
    (actions',state') <- guiGetActionsAndState gui
    return $ actionGetByGroupAndId state' actions' groupName idName
{-# INLINEABLE getActionByGroupAndId #-}

-- | Найти действи__я__ по имени группы и идентификатору в группе.
-- Возвращаются только видимые в настоящий момент действия (в зависимости от кода состояния программы),
-- вместе с именами их группы и идентификатора.
-- Запрос сразу многих Action-ов немного уменьшает время выполнения.
getActionsByGroupAndId  :: MonadIO m =>
    Gui -> -- ^ Ссылка на GUI приложения.
    V.Vector (ByteString,ByteString) -> -- ^ Вектор запрашиваемых Action-ов по именам групп и идентификаторам.
    m (V.Vector (ByteString,ByteString,Action))
getActionsByGroupAndId gui v = do
    (actions',state') <- guiGetActionsAndState gui
    return $ V.map (\(g,i,a) -> (g,i,fromJust a)) $ V.filter (\(_,_,a) -> isJust a) $
        V.map (\(g,i) -> (g,i,actionGetByGroupAndId state' actions' g i)) v

-- | Изменить значение поля @GUI.BaseLayer.Depend1.Action.actionEnable@  действия,
-- определяемого по имени группы и идентификатору в группе.
setActionEnable :: MonadIO m => Gui -> -- ^ Ссылка на GUI приложения.
                                ByteString -> -- ^ Имя группы.
                                ByteString -> -- ^ Идентификатор действия в группе.
                                Bool -> -- ^ Новое значение.
                                m ()
setActionEnable gui groupName idName b =
    guiUpdateActions gui (actionSetEnable b groupName idName)
{-# INLINEABLE setActionEnable #-}

-- | Изменить функцию действия, определяемого  по имени группы и идентификатору в группе.
setAction :: MonadIO m => Gui -> -- ^ Ссылка на GUI приложения.
                          ByteString -> -- ^ Имя группы.
                          ByteString -> -- ^ Идентификатор действия в группе.
                          (forall n. MonadIO n => n ()) -> -- ^ Новая функция для Action-а.
                          m ()
setAction gui groupName idName f =
    guiUpdateActions gui (actionSetOnAction f groupName idName)
{-# INLINEABLE setAction #-}

-- | Найти видимые сейчас Action-ы указанной группы вместе с их идентификаторами.
getVisibleActions :: MonadIO m => Gui -> -- ^ Ссылка на GUI приложения.
                                  ByteString -> -- ^ Имя группы.
                                  m (V.Vector (ByteString,Action))
getVisibleActions gui groupName = do
    GUIRecord{..} <- readMonadIORef gui
    return $ actionGetVisibles guiState guiActions groupName
{-# INLINEABLE getVisibleActions #-}
