{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
module GUI.BaseLayer.Action(
    -- GUI.BaseLayer.Action.Internal.Action
    pattern AllInvisibleActionMask,pattern AllDisableActionMask,pattern AllEnableActionMask
    ,HotKeyModifier(..),HotKey(..),ActionMask(..),GuiState(..),ActionValue(..),Action(..),Actions
    ,ActionState(..),ActionId(..),ActionFn
    ,fromActionMask,getActionValueState,isVisibleActionState
    ,mkActionMask,mkEmptyActions
    -- GUI.BaseLayer.Action
    ,hkNoModifier,hkCtrl,hkShift,hkAlt,hkCtrlShift,hkCtrlAlt,hkShiftAlt
    ,scaToHotKeyModifier,addActions,chkHotKey,getActionByGroupAndId,getActionsByGroupAndId,setActionEnable
    ,setAction,getVisibleActions
    ) where

import Control.Monad.IO.Class -- (MonadIO)
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified SDL
import GUI.BaseLayer.Internal.Action
import GUI.BaseLayer.Internal.Types
import GUI.BaseLayer.Ref
import GUI.BaseLayer.Keyboard

hkNoModifier :: SDL.Keycode -> Maybe HotKey
hkNoModifier = Just . HotKey HotKeyNoModifier
{-# INLINE hkNoModifier #-}

hkCtrl :: SDL.Keycode -> Maybe HotKey
hkCtrl = Just . HotKey HotKeyCtrl
{-# INLINE hkCtrl #-}

hkShift :: SDL.Keycode -> Maybe HotKey
hkShift = Just . HotKey HotKeyShift
{-# INLINE hkShift #-}

hkAlt :: SDL.Keycode -> Maybe HotKey
hkAlt = Just . HotKey HotKeyAlt
{-# INLINE hkAlt #-}

hkCtrlShift :: SDL.Keycode -> Maybe HotKey
hkCtrlShift = Just . HotKey HotKeyCtrlShift
{-# INLINE hkCtrlShift #-}

hkCtrlAlt :: SDL.Keycode -> Maybe HotKey
hkCtrlAlt = Just . HotKey HotKeyCtrlAlt
{-# INLINE hkCtrlAlt #-}

hkShiftAlt :: SDL.Keycode -> Maybe HotKey
hkShiftAlt = Just . HotKey HotKeyShiftAlt
{-# INLINE hkShiftAlt #-}

scaToHotKeyModifier :: ShiftCtrlAlt -> HotKeyModifier
scaToHotKeyModifier ShiftCtrlAlt{isShift= False, isCtrl= True, isAlt= False  } = HotKeyCtrl
scaToHotKeyModifier ShiftCtrlAlt{isShift= True, isCtrl= False, isAlt= False } = HotKeyShift
scaToHotKeyModifier ShiftCtrlAlt{isShift= False, isCtrl=  False, isAlt=  True} = HotKeyAlt
scaToHotKeyModifier ShiftCtrlAlt{isShift= True, isCtrl=  True, isAlt=  False} = HotKeyCtrlShift
scaToHotKeyModifier ShiftCtrlAlt{isShift= False, isCtrl=  True, isAlt=  True} = HotKeyCtrlAlt
scaToHotKeyModifier ShiftCtrlAlt{isShift= True, isCtrl= False, isAlt=  True} = HotKeyShiftAlt
scaToHotKeyModifier _ = HotKeyNoModifier
{-# INLINE scaToHotKeyModifier #-}

-- actionAdd :: T.Text -> [(T.Text,Action)] -> Actions -> Actions
addActions :: MonadIO m => Gui -> T.Text -> [(T.Text,Action)] -> m ()
addActions gui groupName lst = modifyMonadIORef' gui (\a -> a{guiActions=actionAdd groupName lst $ guiActions a})
{-# INLINE addActions #-}

-- actionFindByHotKey :: GuiState -> Actions -> HotKey -> Maybe ActionFn
chkHotKey :: MonadIO m => Gui -> HotKeyModifier -> SDL.Keycode -> m Bool
chkHotKey gui km key = do
    g <- readMonadIORef gui
    case actionFindByHotKey (guiState g) (guiActions g) $ HotKey km key of
        Just a -> actionFn a >> return True
        _ -> return False
{-# INLINE chkHotKey #-}

--actionGetByGroupAndId :: GuiState ->  Actions -> T.Text -> T.Text -> Maybe Action
getActionByGroupAndId  :: MonadIO m => Gui -> T.Text -> T.Text -> m (Maybe Action)
getActionByGroupAndId gui groupName idName = do
    GUIStruct{..} <- readMonadIORef gui
    return $ actionGetByGroupAndId guiState guiActions groupName idName
{-# INLINE getActionByGroupAndId #-}

getActionsByGroupAndId  :: MonadIO m => Gui -> V.Vector (T.Text,T.Text) -> m (V.Vector (T.Text,T.Text,Action))
getActionsByGroupAndId gui v = do
    GUIStruct{..} <- readMonadIORef gui
    return $ V.map (\(g,i,a) -> (g,i,fromJust a)) $ V.filter (\(_,_,a) -> isJust a) $
        V.map (\(g,i) -> (g,i,actionGetByGroupAndId guiState guiActions g i)) v

-- actionSetEnable :: Actions -> T.Text -> T.Text -> Bool -> Actions
setActionEnable :: MonadIO m => Gui -> T.Text -> T.Text -> Bool -> m ()
setActionEnable gui groupName idName b =
    modifyMonadIORef' gui (\a -> a{guiActions=actionSetEnable (guiActions a) groupName idName b})

setAction :: MonadIO m => Gui -> T.Text -> T.Text -> (forall m. MonadIO m => m ()) -> m ()
setAction gui groupName idName f =
    modifyMonadIORef' gui (\a -> a{guiActions=actionSetOnAction (guiActions a) groupName idName f})
{-# INLINE setAction #-}

-- actionGetVisibles :: GuiState -> Actions -> T.Text -> V.Vector Action
getVisibleActions :: MonadIO m => Gui -> T.Text -> m (V.Vector (T.Text,Action))
getVisibleActions gui groupName = do
    GUIStruct{..} <- readMonadIORef gui
    return $ actionGetVisibles guiState guiActions groupName
{-# INLINE getVisibleActions #-}
