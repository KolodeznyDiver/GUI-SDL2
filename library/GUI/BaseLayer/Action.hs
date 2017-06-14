{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module GUI.BaseLayer.Action(
    -- GUI.BaseLayer.Action.Internal.Action
    pattern AllInvisibleActionMask,pattern AllDisableActionMask,pattern AllEnableActionMask
    ,ActionMask(..),GuiState(..),ActionValue(..),Action(..),Actions
    ,ActionState(..),ActionId(..),ActionFn
    ,fromActionMask,getActionValueState,isVisibleActionState
    ,mkActionMask,mkEmptyActions
    -- GUI.BaseLayer.Action
    ,kNoModifier,kCtrl,kShift,kAlt,kCtrlShift,kCtrlAlt,kShiftAlt
    ,addActions,chkHotKey,getActionByGroupAndId,getActionsByGroupAndId,setActionEnable
    ,setAction,getVisibleActions
    ) where

import Data.Monoid
import Control.Monad.IO.Class -- (MonadIO)
import Data.Maybe
import qualified Data.Vector as V
import           Data.ByteString.Char8   (ByteString)
import           TextShow (showb)
import qualified SDL
import GUI.BaseLayer.Internal.Action
import GUI.BaseLayer.Internal.Types
import GUI.BaseLayer.Ref
import GUI.BaseLayer.Keyboard
import GUI.BaseLayer.Logging

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

addActions :: MonadIO m => Gui -> ByteString -> [(ByteString,Action)] -> m ()
addActions gui groupName lst = modifyMonadIORef' gui (\a -> a{guiActions=actionAdd groupName lst $ guiActions a})
{-# INLINE addActions #-}

chkHotKey :: MonadIO m => Gui -> KeyModifiers -> SDL.Keycode -> m Bool
chkHotKey gui km key = do
    g <- readMonadIORef gui
    let k = KeyWithModifiers km key
    case actionFindByHotKey (guiState g) (guiActions g) k of
        Just a -> logOnErr (guiLog g) ("HotKey " <> showb k)
                    (actionFn a) >> return True
        _ -> return False
{-# INLINE chkHotKey #-}

getActionByGroupAndId  :: MonadIO m => Gui -> ByteString -> ByteString -> m (Maybe Action)
getActionByGroupAndId gui groupName idName = do
    GUIStruct{..} <- readMonadIORef gui
    return $ actionGetByGroupAndId guiState guiActions groupName idName
{-# INLINE getActionByGroupAndId #-}

getActionsByGroupAndId  :: MonadIO m => Gui -> V.Vector (ByteString,ByteString) ->
                                m (V.Vector (ByteString,ByteString,Action))
getActionsByGroupAndId gui v = do
    GUIStruct{..} <- readMonadIORef gui
    return $ V.map (\(g,i,a) -> (g,i,fromJust a)) $ V.filter (\(_,_,a) -> isJust a) $
        V.map (\(g,i) -> (g,i,actionGetByGroupAndId guiState guiActions g i)) v

setActionEnable :: MonadIO m => Gui -> ByteString -> ByteString -> Bool -> m ()
setActionEnable gui groupName idName b =
    modifyMonadIORef' gui (\a -> a{guiActions=actionSetEnable (guiActions a) groupName idName b})

setAction :: MonadIO m => Gui -> ByteString -> ByteString -> (forall n. MonadIO n => n ()) -> m ()
setAction gui groupName idName f =
    modifyMonadIORef' gui (\a -> a{guiActions=actionSetOnAction (guiActions a) groupName idName f})
{-# INLINE setAction #-}

getVisibleActions :: MonadIO m => Gui -> ByteString -> m (V.Vector (ByteString,Action))
getVisibleActions gui groupName = do
    GUIStruct{..} <- readMonadIORef gui
    return $ actionGetVisibles guiState guiActions groupName
{-# INLINE getVisibleActions #-}
