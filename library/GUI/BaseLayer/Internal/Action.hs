{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
module GUI.BaseLayer.Internal.Action(
    pattern AllInvisibleActionMask,pattern AllDisableActionMask,pattern AllEnableActionMask
    ,HotKeyModifier(..),HotKey(..),ActionMask(..),GuiState(..),ActionValue(..)
    ,ActionState(ActionInvisible,ActionDisable,ActionEnable),ActionId(..),Action(..)
    ,ActionCollection,HotKeyCollection,Actions(..)
    ,ActionFn(..)
    ,isVisibleActionState,fromActionMask,getActionValueState
    ,mkActionMask,mkEmptyActions,actionAdd,actionFindByHotKey,actionGetByGroupAndId,actionDelByGroupAndId
    ,actionSetEnable,actionSetOnAction,actionGetVisibles

    ) where

import Data.Maybe
import Data.Bits
import Control.Monad.IO.Class (MonadIO)
import Data.Word
import Control.Monad.Plus (partial)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Default
import qualified SDL
import GUI.BaseLayer.Keyboard

pattern AllInvisibleActionMask :: ActionMask
pattern AllDisableActionMask :: ActionMask
pattern AllEnableActionMask :: ActionMask
pattern AllInvisibleActionMask = ActionMask 0x0000000000000000
pattern AllDisableActionMask   = ActionMask 0xAAAAAAAAAAAAAAAA
pattern AllEnableActionMask    = ActionMask 0xFFFFFFFFFFFFFFFF

data HotKeyModifier = HotKeyNoModifier
                    | HotKeyCtrl
                    | HotKeyShift
                    | HotKeyAlt
                    | HotKeyCtrlShift
                    | HotKeyCtrlAlt
                    | HotKeyShiftAlt
                    deriving (Eq, Ord)

instance Show HotKeyModifier where
    show HotKeyNoModifier = ""
    show HotKeyCtrl = "Ctrl-"
    show HotKeyShift = "Shift-"
    show HotKeyAlt = "Alt-"
    show HotKeyCtrlShift = "Ctrl-Shift-"
    show HotKeyCtrlAlt = "Ctrl-Alt-"
    show HotKeyShiftAlt = "Shift-Alt-"


data HotKey = HotKey HotKeyModifier SDL.Keycode
                    deriving (Eq, Ord)

instance Show HotKey where
    show (HotKey km k) = show km ++ showKeycode k

newtype ActionMask = ActionMask { unActionMask :: Word64}

newtype GuiState = GuiState { unGuiState :: Int}
                            deriving (Eq)

data ActionValue = ActionValue  { actionMask :: ActionMask
                                , actionEnable :: Bool
                                , onAction :: forall m. MonadIO m => m ()
                                }

instance Default ActionValue where
    def = ActionValue  { actionMask = AllEnableActionMask
                       , actionEnable = True
                       , onAction = return ()
                       }

data ActionState = ActionInvisible | ActionReserved | ActionDisable | ActionEnable
                    deriving (Eq, Enum)
{--}
data ActionId = ActionId    { actGroup :: T.Text
                            , actKey :: T.Text
                            }
                            deriving (Eq)

data Action = Action    { actionText :: T.Text
                        , actionHint  :: T.Text
                        , actionPicture  :: T.Text
                        , actionHotKey :: Maybe HotKey
                        , actionValue :: ActionValue
                        }

instance Default Action where
    def = Action { actionText = T.empty
                 , actionHint = T.empty
                 , actionPicture = T.empty
                 , actionHotKey = Nothing
                 , actionValue = def
                 }

type ActionCollection = HM.HashMap T.Text (HM.HashMap T.Text Action)
type HotKeyCollection = Map.Map HotKey ActionValue

data Actions = Actions  { actions :: ActionCollection
                        , actsHotKeys :: HotKeyCollection
                        }

newtype ActionFn = ActionFn {actionFn :: forall m. MonadIO m => m ()}


isVisibleActionState :: ActionState -> Bool
isVisibleActionState ActionDisable = True
isVisibleActionState ActionEnable  = True
isVisibleActionState _ = False
{-# INLINE isVisibleActionState #-}

isVisibleAction :: GuiState ->  Action -> Bool
isVisibleAction s = isVisibleActionState . getActionValueState s . actionValue
{-# INLINE isVisibleAction #-}

fromActionMask ::  GuiState ->  ActionMask -> ActionState
fromActionMask s a = toEnum $ fromIntegral $ 3 .&. (unActionMask a `unsafeShiftR` (2 * unGuiState s) )
{-# INLINE fromActionMask #-}

getActionValueState :: GuiState -> ActionValue -> ActionState
getActionValueState s a = case fromActionMask s $ actionMask a of
                            ActionDisable -> ActionDisable
                            ActionEnable -> if actionEnable a then ActionEnable else ActionDisable
                            _ -> ActionInvisible
{-# INLINE getActionValueState #-}

updateActionMask ::  GuiState -> ActionState -> ActionMask -> ActionMask
updateActionMask s n am = let nShft = 2 * unGuiState s
                          in ActionMask ((fromIntegral (fromEnum n) `unsafeShiftL` nShft) .|.
                                        (unActionMask am .&. complement (3 `unsafeShiftL` nShft)))

mkActionMask ::  [(GuiState,ActionState)] -> ActionMask
mkActionMask = foldr (uncurry updateActionMask) AllInvisibleActionMask
{-# INLINE mkActionMask #-}

mkEmptyActions :: Actions
mkEmptyActions = Actions HM.empty Map.empty
{-# INLINE mkEmptyActions #-}

-- remove onlu from actions, not from actsHotKeys
removeHotkeysFromActions :: Actions -> V.Vector HotKey -> ActionCollection
removeHotkeysFromActions Actions{..} v =
    let setHK = V.foldr (\k s -> if Map.member k actsHotKeys then Set.insert k s else s) Set.empty v in
    HM.map (HM.map (\a -> maybe a
            (\k -> if Set.member k setHK then a{actionHotKey=Nothing} else a) $ actionHotKey a)) actions

actionAdd :: T.Text -> [(T.Text,Action)] -> Actions -> Actions
actionAdd groupName lst acts@Actions{..} =
    let aWithHK = filter (\(_,Action{..}) -> isJust actionHotKey) lst
        a = removeHotkeysFromActions acts $ V.map (\(_,Action{..}) -> fromJust actionHotKey) $
                V.fromList aWithHK
        h = HM.fromList lst
    in Actions
        (HM.insert groupName (maybe h (HM.union h) $ HM.lookup groupName a) a)
        (Map.union (Map.fromList $ map (\(_,Action{..}) ->(fromJust actionHotKey,actionValue)) aWithHK) actsHotKeys)

actionFindByHotKey :: GuiState -> Actions -> HotKey -> Maybe ActionFn
actionFindByHotKey s a k = case Map.lookup k $ actsHotKeys a of
                                Just v | ActionEnable == getActionValueState s v ->
                                                Just $ ActionFn $ onAction v
                                _ -> Nothing
{-# INLINE actionFindByHotKey #-}

actionGetByGroupAndId :: GuiState ->  Actions -> T.Text -> T.Text -> Maybe Action
actionGetByGroupAndId s a g i = HM.lookup g (actions a) >>= HM.lookup i >>= partial (isVisibleAction s)
{-# INLINE actionGetByGroupAndId #-}

removeHotkeyFromMap :: Maybe HotKey -> HotKeyCollection -> HotKeyCollection
removeHotkeyFromMap (Just k) c = Map.delete k c
removeHotkeyFromMap _ c = c
{-# INLINE removeHotkeyFromMap #-}

actionDelByGroupAndId :: T.Text -> T.Text -> Actions -> Actions
actionDelByGroupAndId groupName idName acts@Actions{..} =
    case HM.lookup groupName actions of
        Just g -> case HM.lookup idName g of
                    Just Action{..} ->
                        Actions
                            (HM.adjust (HM.delete idName) groupName actions)
                            (removeHotkeyFromMap actionHotKey actsHotKeys)
                    _ -> acts
        _ -> acts
{-# INLINE actionDelByGroupAndId #-}

-- no exported fun. Suitable for modifying any fields except actionHotKey
actionUpdateByGroupAndIdUnsafe :: (Action -> Action) -> Actions -> T.Text -> T.Text -> Actions
actionUpdateByGroupAndIdUnsafe f Actions{..} g i =
    Actions
        (HM.adjust (HM.adjust f i) g actions)
        actsHotKeys -- no modify even if HotKey was changed
{-# INLINE actionUpdateByGroupAndIdUnsafe #-}

-- example field modification
actionSetEnable :: Actions -> T.Text -> T.Text -> Bool -> Actions
actionSetEnable acts g i b =
    actionUpdateByGroupAndIdUnsafe (\a -> a{actionValue=(actionValue a){actionEnable=b}}) acts g i
{-# INLINE actionSetEnable #-}

actionSetOnAction :: Actions -> T.Text -> T.Text -> (forall m. MonadIO m => m ()) -> Actions
actionSetOnAction acts g i f =
    actionUpdateByGroupAndIdUnsafe (\a -> a{actionValue=(actionValue a){onAction=f}}) acts g i
{-# INLINE actionSetOnAction #-}

actionGetVisibles :: GuiState -> Actions -> T.Text -> V.Vector (T.Text,Action)
actionGetVisibles s Actions{..} groupName =
    maybe V.empty (V.filter (isVisibleAction s . snd) . V.fromList . HM.toList) $ HM.lookup groupName actions
{-# INLINE actionGetVisibles #-}
