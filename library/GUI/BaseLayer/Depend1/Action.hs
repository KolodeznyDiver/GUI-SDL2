{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module:      GUI.BaseLayer.Depend1.Action
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Этот модуль, совместно с "GUI.BaseLayer.Action", реализует поддержку так называемых Action's -
-- Действий в GUI. Код разделён на два модуля для уменьшения числа зависимостей между модулями.
--
-- Функции из этого модуля не предназначены для вызова из кода вне GUI.BaseLayer.

module GUI.BaseLayer.Depend1.Action(
    -- * Код состояния приложения.

    -- $GuiState

    GuiState(..)

    -- * ActionState

    -- $ActionState

    ,ActionState(ActionInvisible,ActionDisable,ActionEnable),isVisibleActionState

    -- * ActionMask

    -- $ActionMask

    ,ActionMask(..),fromActionMask,updateActionMask,mkActionMask
    ,pattern AllInvisibleActionMask,pattern AllDisableActionMask,pattern AllEnableActionMask

    -- * ActionValue

    -- $ActionValue

    ,ActionValue(..),getActionValueState,HotKeyCollection

    -- * Action

    -- $Action

    ,Action(..),Actions
    ,actionGetByGroupAndId,ActionFn(..),actionFindByHotKey,actionGetVisibles
    ,mkEmptyActions,actionAdd,actionDelByGroupAndId,actionSetEnable,actionSetOnAction
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
import           Data.ByteString.Char8   (ByteString)
import Data.Default
import GUI.BaseLayer.Depend0.Keyboard

pattern AllInvisibleActionMask :: ActionMask
pattern AllDisableActionMask :: ActionMask
pattern AllEnableActionMask :: ActionMask
pattern AllInvisibleActionMask = ActionMask 0x0000000000000000
pattern AllDisableActionMask   = ActionMask 0xAAAAAAAAAAAAAAAA
pattern AllEnableActionMask    = ActionMask 0xFFFFFFFFFFFFFFFF

-- $GuiState
-- Код состояния приложения : 'GuiState' – число 0..31 в обёртке из /newtype/, одно на весь GUI, изменяемое в
-- пользовательском коде с помощью функции @GUI.BaseLayer.Core.setGuiState@.
-- При изменении состояния приложения изменяются состояния видимости Action-ов, и, таким, образом,
-- могут изменяться меню, toolbar-ы (в будущих версиях), горячие клавишы.
-- Например, в редакторе или IDE состав пунктов меню зависит от того, открыт документ (или проект) или нет.
-- Меню описываются со всеми пунктами, но видны будут те, которые настроены под данное состояние программы.
-- Т.е. меняем код состояния программы и меняются меню, хоткеи и т.д.

-- У каждого Action-а есть поле задающее его состояние (видим, недоступен, доступен) для каждого состояния программы. Например, в редакторе или IDE состав пунктов меню зависит от того, открыт документ (или проект) или нет. Меню описываются со всеми пунктами, но видны будут те, которые настроены под данное состояние программы. Т.е. меняем код состояния программы и меняются меню, хоткеи и т.д.

-- | Код состояния приложения – число 0..31 в обёртке из /newtype/.
newtype GuiState = GuiState { unGuiState :: Int}
                            deriving (Eq)

-- $ActionState
-- | Состояние определённого Action-а при определённом коде состояния программы
-- Хранится как два бита маски состояний Action-а.

-- | Состояние определённого Action-а при определённом коде состояния программы.
data ActionState =
      ActionInvisible -- ^ Action не видет и не активен.
    | ActionReserved  -- ^ Зарезервировано.
    | ActionDisable   -- ^ Action видим но не активен (недоступен).
    | ActionEnable    -- ^ Action видим и активен (доступен).
    deriving (Eq, Enum)

-- | Возвращает состояние видимости из 'ActionState'.
isVisibleActionState :: ActionState -> Bool
isVisibleActionState ActionDisable = True
isVisibleActionState ActionEnable  = True
isVisibleActionState _ = False
{-# INLINE isVisibleActionState #-}

-- $ActionMask
-- 'ActionMask' хранится как поле Word64 в каждом Action-е. Оно содержит коды соответствующие 'ActionState',
-- по 2 бита на один 'ActionState'. Младшие два бита маски (нулевой и первый) соотвествуют 'ActionState'
-- для нулевого состояния программы, следующие два бита (второй и третий) для состояния программы 1, и.т.д.

-- | Тип маски состояний в обёртке из /newtype/.
newtype ActionMask = ActionMask { unActionMask :: Word64}

-- | Получить 'ActionState' для указаного состояния программы 'GuiState'.
fromActionMask ::  GuiState ->  ActionMask -> ActionState
fromActionMask s a = toEnum $ fromIntegral $ 3 .&. (unActionMask a `unsafeShiftR` (2 * unGuiState s) )
{-# INLINE fromActionMask #-}

-- | Заменить в маске состояний состояние для указанного 'GuiState'.
updateActionMask ::  GuiState -> ActionState -> ActionMask -> ActionMask
updateActionMask s n am = let nShft = 2 * unGuiState s
                          in ActionMask ((fromIntegral (fromEnum n) `unsafeShiftL` nShft) .|.
                                        (unActionMask am .&. complement (3 `unsafeShiftL` nShft)))

-- | Создать маску состояний из списка пар [(Состояние программы,Состояние Action-а)].
-- Для незаданных в списке состояний программы с остояние Action-а будет @ActionInvisible@.
mkActionMask ::  [(GuiState,ActionState)] -> ActionMask
mkActionMask = foldr (uncurry updateActionMask) AllInvisibleActionMask
{-# INLINE mkActionMask #-}

-- $ActionValue
-- 'ActionValue' - тип общий для собственно 'Action' (входит в него как поле @actionValue@),
-- так же используется и как значения коллекции (Map, словаря) горячих клавиш.

-- | Тип поля @actionValue@, а так же значений @HotKeyCollection@.
data ActionValue = ActionValue {
      actionMask :: ActionMask -- ^ Маска состояний.
    , actionEnable :: Bool  -- ^ Позволяет дополнительно запретить Action\/горячую клавишу в независимости от
                            -- @actionMask@. Т.е. если это поле False, то состояние будет ActionDisable
                            -- если согласно маски оно ActionEnable.
    , onAction :: forall m. MonadIO m => m ()  -- ^ Функция вызываемая для Action-а или горячей клавишы.
                               }

instance Default ActionValue where
    def = ActionValue  { actionMask = AllEnableActionMask
                       , actionEnable = True
                       , onAction = return ()
                       }

-- | Получить 'ActionState' для указаного состояния программы 'GuiState' из 'ActionValue' c
-- учётом его полей @actionMask@ и @actionEnable@.
getActionValueState :: GuiState -> ActionValue -> ActionState
getActionValueState s a = case fromActionMask s $ actionMask a of
                            ActionDisable -> ActionDisable
                            ActionEnable -> if actionEnable a then ActionEnable else ActionDisable
                            _ -> ActionInvisible
{-# INLINEABLE getActionValueState #-}

-- | Коллекция горячих ключей приложения. Тип поля @actsHotKeys@.
type HotKeyCollection = Map.Map KeyWithModifiers ActionValue

-- $Action
-- 'Action' - параметры Действия. Action-ы собраны в двухуровневый контейнер -
-- разбиты по группам. Action в системе идентифицируется по имени группы и его собственному имени.
-- Имена групп и идентификаторов самих Action-ов 'Data.ByteString.Char8.ByteString', а не
-- 'Data.Text.Text', как ключи менеджера ресурсов (См. "GUI.BaseLayer.Resource").
-- Дело в том, что ключи менеджера ресурсов могут быть одновременно именем файла и в них допустимы
-- национальные символы, а группы и идентификаторы Action-ов не ассоциированы с файловой ситемой,
-- в их строках предпочтительно использовать только символя /ASCII/.

-- | 'Action' - параметры одного Действия.
data Action = Action {
      actionText :: T.Text -- ^ Текст Action-а, например, текст пункта меню.
    , actionHint  :: T.Text -- ^ Текст подсказки.
    , actionPicture  :: T.Text -- ^ Имя графического файла с картинкой для Action-а.
    , actionHotKey :: Maybe KeyWithModifiers -- ^ Горячая клавиша для Action-а.
    , actionValue :: ActionValue -- ^ Остальные параметры Action-а, см. 'ActionValue'.
                     }

instance Default Action where
    def = Action { actionText = T.empty
                 , actionHint = T.empty
                 , actionPicture = T.empty
                 , actionHotKey = Nothing
                 , actionValue = def
                 }

-- | Возвращает состояние видимости из 'GuiState' и 'Action'.
isVisibleAction :: GuiState ->  Action -> Bool
isVisibleAction s = isVisibleActionState . getActionValueState s . actionValue
{-# INLINEABLE isVisibleAction #-}

-- | Коллекция действий. Хеш таблица групп действий, значения которой - хеш таблицы действий.
-- Я слегка \"перенаворочал\"? Возможно. А возможно нет.
type ActionCollection = HM.HashMap ByteString (HM.HashMap ByteString Action)

-- | Все действия и горячие клавишы в системе. Тип поля @GUI.BaseLayer.Types.guiActions@.
data Actions = Actions {
      actions :: ActionCollection -- ^ Action-ы
    , actsHotKeys :: HotKeyCollection -- ^ Таблица горячих клавиш. Нужна для ускорения поиска
                                      -- Action-а по горячей клавише.
                        }

-- | Создание пустого 'Actions'.
mkEmptyActions :: Actions
mkEmptyActions = Actions HM.empty Map.empty
{-# INLINE mkEmptyActions #-}

-- | No exported.
-- Remove hotyeys only from actions, not from actsHotKeys.
removeHotkeysFromActions :: Actions -> V.Vector KeyWithModifiers -> ActionCollection
removeHotkeysFromActions Actions{..} v =
    let setHK = V.foldr (\k s -> if Map.member k actsHotKeys then Set.insert k s else s) Set.empty v in
    HM.map (HM.map (\a -> maybe a
            (\k -> if Set.member k setHK then a{actionHotKey=Nothing} else a) $ actionHotKey a)) actions

-- | Добавить, дополнить или заменить действия в одной группе. Если группы нет, она создаётся.
actionAdd :: ByteString -> -- ^ Имя группы.
    [(ByteString,Action)] -> -- ^ Список новых действий с их строковыми идентификаторами.
    Actions -> -- ^ Исходное состояние 'Actions'.
    Actions
actionAdd groupName lst acts@Actions{..} =
    let aWithHK = filter (\(_,Action{..}) -> isJust actionHotKey) lst
        a = removeHotkeysFromActions acts $ V.map (\(_,Action{..}) -> fromJust actionHotKey) $
                V.fromList aWithHK
        h = HM.fromList lst
    in Actions
        (HM.insert groupName (maybe h (HM.union h) $ HM.lookup groupName a) a)
        (Map.union (Map.fromList $ map (\(_,Action{..}) ->(fromJust actionHotKey,actionValue)) aWithHK) actsHotKeys)

-- | Вспомогательный тип - обёртка для возврата действия как полиморфной функции.
newtype ActionFn = ActionFn {actionFn :: forall m. MonadIO m => m ()}

-- | Найти 'Action' по горячей клавише.
actionFindByHotKey ::
    GuiState -> -- ^ Код состояния программы.
    Actions -> -- ^ 'Actions' в котором производится поиск (в GUI существует только один 'Actions').
    KeyWithModifiers -> -- ^ Горячая клавиша.
    Maybe ActionFn
actionFindByHotKey s a k = case Map.lookup k $ actsHotKeys a of
                                Just v | ActionEnable == getActionValueState s v ->
                                                Just $ ActionFn $ onAction v
                                _ -> Nothing
{-# INLINE actionFindByHotKey #-}

-- | Найти действие по имени группы и идентификатору в группе.
-- Возвращаются только видимые в настоящий момент действия.
actionGetByGroupAndId :: GuiState -> -- ^ Код состояния программы.
                        Actions -> -- ^ 'Actions'.
                        ByteString -> -- ^ Имя группы.
                        ByteString -> -- ^ Идентификатор действия в группе.
                        Maybe Action
actionGetByGroupAndId s a g i = HM.lookup g (actions a) >>= HM.lookup i >>= partial (isVisibleAction s)
{-# INLINEABLE actionGetByGroupAndId #-}

-- | Найти видимые сейчас Action-ы указанной группы вместе с их идентификаторами.
actionGetVisibles :: GuiState -> -- ^ Код состояния программы.
                     Actions -> -- ^ 'Actions'.
                     ByteString -> -- ^ Имя группы.
                     V.Vector (ByteString,Action)
actionGetVisibles s Actions{..} groupName =
    maybe V.empty (V.filter (isVisibleAction s . snd) . V.fromList . HM.toList) $ HM.lookup groupName actions
{-# INLINEABLE actionGetVisibles #-}


-- | No exported.
removeHotkeyFromMap :: Maybe KeyWithModifiers -> HotKeyCollection -> HotKeyCollection
removeHotkeyFromMap (Just k) c = Map.delete k c
removeHotkeyFromMap _ c = c
{-# INLINEABLE removeHotkeyFromMap #-}

-- | Удалить действие по имени группы и идентификатору в группе.
actionDelByGroupAndId :: ByteString -> -- ^ Имя группы.
                         ByteString -> -- ^ Идентификатор действия в группе.
                         Actions -> -- ^ Исходное состояние 'Actions'.
                         Actions
actionDelByGroupAndId groupName idName acts@Actions{..} =
    case HM.lookup groupName actions of
        Just g -> case HM.lookup idName g of
                    Just Action{..} ->
                        Actions
                            (HM.adjust (HM.delete idName) groupName actions)
                            (removeHotkeyFromMap actionHotKey actsHotKeys)
                    _ -> acts
        _ -> acts

-- no exported fun. Suitable for modifying any fields except actionHotKey
actionUpdateByGroupAndIdUnsafe :: (Action -> Action) ->
                                  (ActionValue -> ActionValue) ->
                                  ByteString ->
                                  ByteString ->
                                  Actions ->
                                  Actions
actionUpdateByGroupAndIdUnsafe f fv g i Actions{..} =
    Actions
        (HM.adjust (HM.adjust f i) g actions)
        (case HM.lookup g actions >>= HM.lookup i of
            Just Action{actionHotKey=Just k} -> Map.adjust fv k actsHotKeys
            _ -> actsHotKeys  )
{-# INLINEABLE actionUpdateByGroupAndIdUnsafe #-}

-- no exported fun. Suitable for modifying any fields except actionHotKey
actionUpdateByGroupAndIdUnsafe':: (ActionValue -> ActionValue) ->
                                  ByteString ->
                                  ByteString ->
                                  Actions ->
                                  Actions
actionUpdateByGroupAndIdUnsafe' fv =
    actionUpdateByGroupAndIdUnsafe (\a -> a{actionValue= fv $ actionValue a}) fv
{-# INLINE actionUpdateByGroupAndIdUnsafe' #-}

-- | Изменить значение поля @actionEnable@  действия, определяемого по имени группы и идентификатору в группе.
actionSetEnable :: Bool -> -- ^ Новое значение для поля @actionEnable@.
                   ByteString -> -- ^ Имя группы.
                   ByteString -> -- ^ Идентификатор действия в группе.
                   Actions -> -- ^ Исходное состояние 'Actions'.
                   Actions
actionSetEnable b = actionUpdateByGroupAndIdUnsafe' (\a -> a{actionEnable=b})
{-# INLINE actionSetEnable #-}

-- | Изменить функцию действия, определяемого  по имени группы и идентификатору в группе.
actionSetOnAction :: (forall m. MonadIO m => m ()) -> -- ^ Новая функция для Action-а.
                     ByteString -> -- ^ Имя группы.
                     ByteString -> -- ^ Идентификатор действия в группе.
                     Actions -> -- ^ Исходное состояние 'Actions'.
                     Actions
actionSetOnAction f = actionUpdateByGroupAndIdUnsafe' (\a -> a{onAction=f})
{-# INLINE actionSetOnAction #-}

