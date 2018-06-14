{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:      GUI.BaseLayer.Depend0.Keyboard
-- Copyright:   (c) 2017-2018 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Вспомогательные функции работы с клавиатурой.
--
--

module GUI.BaseLayer.Depend0.Keyboard(
    ShiftCtrlAlt(..),getShftCtrlAlt,isEnterKey,showbKeycode,KeyModifiers(..),KeyWithModifiers(..)
    ,scaToKeyModifiers,getActualShiftCtrlAlt,getActualKeyModifiers
    ) where

import Data.Monoid
import Control.Monad.IO.Class
import GHC.Generics (Generic)
import Data.Data (Data)
import Data.Typeable
import TextShow
import TextShow.Data.Char
import TextShow.Data.Integral
import qualified SDL
import Data.Char

-- | Состояние клавиш Shift, Ctrl и Alt. Тип сделан для упрощения информации о состоянии клавиатуры
-- по сравнению с 'SDL.KeyModifier'.
data ShiftCtrlAlt = ShiftCtrlAlt { isShift :: Bool
                                 , isCtrl  :: Bool
                                 , isAlt   :: Bool
                                 }
                                 deriving (Eq)

-- | Функция отображает тип 'SDL.KeyModifier' в 'ShiftCtrlAlt'.
-- При этом сокращаются подробности. В типе 'SDL.KeyModifier' находится информация о состоянии двух Shift-ов,
-- трёх Alt-ов и пр.
getShftCtrlAlt :: SDL.KeyModifier -> ShiftCtrlAlt
getShftCtrlAlt km = ShiftCtrlAlt (SDL.keyModifierLeftShift km || SDL.keyModifierRightShift km)
                                 (SDL.keyModifierLeftCtrl  km || SDL.keyModifierRightCtrl km)
                                 (SDL.keyModifierLeftAlt km || SDL.keyModifierRightAlt km || SDL.keyModifierAltGr km)
{-# INLINEABLE getShftCtrlAlt #-}

-- | Код клавишы соответсвует какому либо Enter-у?
isEnterKey :: SDL.Keycode -> Bool
isEnterKey keycode = keycode == SDL.KeycodeReturn || -- keycode == SDL.KeycodeReturn2 ||
                     keycode == SDL.KeycodeCaret || keycode == SDL.KeycodeKPEnter
{-# INLINEABLE isEnterKey #-}

-- | Преобразование кода 'SDL.Keycode' в 'Builder' из "TextShow", т.е. в конечном итоге, в текст.
-- Нужна только для отображений горячих клавиш,
-- по этому, не подходящие для них клавишы закомментированы.
showbKeycode :: SDL.Keycode -> Builder
{-
showbKeycode SDL.KeycodeSpace  = "' '"
showbKeycode SDL.KeycodeBackspace  = "Backspace"
showbKeycode SDL.KeycodeTab = "Tab"
showbKeycode SDL.KeycodeInsert = "Ins"
showbKeycode SDL.KeycodeHome = "Home"
showbKeycode SDL.KeycodePageUp  = "PgUp"
showbKeycode SDL.KeycodeDelete  = "Del"
showbKeycode SDL.KeycodeEnd  = "End"
showbKeycode SDL.KeycodePageDown  = "PgDn"
showbKeycode SDL.KeycodeRight  = "Right"
showbKeycode SDL.KeycodeLeft = "Left"
showbKeycode SDL.KeycodeDown = "Down"
showbKeycode SDL.KeycodeUp  = "Up"
showbKeycode SDL.KeycodeEscape = "Esc" -}
showbKeycode k
--    | isEnterKey k = "Enter"
    | k>= SDL.KeycodeF1 && k<=SDL.KeycodeF12 = singleton 'F' <> showb
        (1 + SDL.unwrapKeycode k - SDL.unwrapKeycode SDL.KeycodeF1)
    | otherwise = case SDL.unwrapKeycode k of
                    key | key<128 -> let c= chr $ fromIntegral key in
                                     if isPrint c then singleton $ toUpper c
                                     else showbLitChar c
                        | otherwise -> showbHex key

-- | Состояние клавиш Shift, Ctrl и Alt. Ещё большее упрощение, чем 'ShiftCtrlAlt',
-- однако, в разных случаях удобно пользоваться тем или другим типом.
data KeyModifiers = KeyNoModifiers
                    | KeyCtrl
                    | KeyShift
                    | KeyAlt
                    | KeyCtrlShift
                    | KeyCtrlAlt
                    | KeyShiftAlt
                    deriving (Bounded, Data, Eq, Ord, Generic, Typeable)

instance Show KeyModifiers where
    show KeyNoModifiers = ""
    show KeyCtrl = "Ctrl-"
    show KeyShift = "Shift-"
    show KeyAlt = "Alt-"
    show KeyCtrlShift = "Ctrl-Shift-"
    show KeyCtrlAlt = "Ctrl-Alt-"
    show KeyShiftAlt = "Shift-Alt-"

instance TextShow KeyModifiers where
    showb = fromString . show

-- | Клавиша вместе с модификаторами. Определяет горячие клавишы.
data KeyWithModifiers = KeyWithModifiers KeyModifiers SDL.Keycode
                    deriving (Data, Eq, Ord, Generic, Typeable)

instance TextShow KeyWithModifiers where
    showb (KeyWithModifiers km k) = showb km <> showbKeycode k
{-
instance Show KeyWithModifiers where
    show = toString . showb
-}

-- | Преобразование из типа 'ShiftCtrlAlt' в 'KeyModifiers'.
scaToKeyModifiers :: ShiftCtrlAlt -> KeyModifiers
scaToKeyModifiers ShiftCtrlAlt{isShift= False, isCtrl= True, isAlt= False  } = KeyCtrl
scaToKeyModifiers ShiftCtrlAlt{isShift= True, isCtrl= False, isAlt= False } = KeyShift
scaToKeyModifiers ShiftCtrlAlt{isShift= False, isCtrl=  False, isAlt=  True} = KeyAlt
scaToKeyModifiers ShiftCtrlAlt{isShift= True, isCtrl=  True, isAlt=  False} = KeyCtrlShift
scaToKeyModifiers ShiftCtrlAlt{isShift= False, isCtrl=  True, isAlt=  True} = KeyCtrlAlt
scaToKeyModifiers ShiftCtrlAlt{isShift= True, isCtrl= False, isAlt=  True} = KeyShiftAlt
scaToKeyModifiers _ = KeyNoModifiers
{-# INLINEABLE scaToKeyModifiers #-}

-- | Текущее состояние клавиш Shift, Ctrl и Alt.
getActualShiftCtrlAlt :: MonadIO m => m ShiftCtrlAlt
getActualShiftCtrlAlt = getShftCtrlAlt <$> SDL.getModState
{-# INLINEABLE getActualShiftCtrlAlt #-}

-- | Текущее состояние клавиш Shift, Ctrl и Alt.
getActualKeyModifiers :: MonadIO m => m KeyModifiers
getActualKeyModifiers = scaToKeyModifiers <$> getActualShiftCtrlAlt
{-# INLINEABLE getActualKeyModifiers #-}
