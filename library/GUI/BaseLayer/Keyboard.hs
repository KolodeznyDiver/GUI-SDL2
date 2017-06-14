{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module GUI.BaseLayer.Keyboard(
    ShiftCtrlAlt(..),getShftCtrlAlt,isEnterKey,showbKeycode,KeyModifiers(..),KeyWithModifiers(..)
    ,scaToKeyModifier
    ) where

import Data.Monoid
import GHC.Generics (Generic)
import Data.Data (Data)
import Data.Typeable
import TextShow
import TextShow.Data.Char
import TextShow.Data.Integral
import qualified SDL
--import Numeric
import Data.Char

data ShiftCtrlAlt = ShiftCtrlAlt { isShift :: Bool
                                 , isCtrl  :: Bool
                                 , isAlt   :: Bool
                                 }
                                 deriving (Eq)

getShftCtrlAlt :: SDL.KeyModifier -> ShiftCtrlAlt
getShftCtrlAlt km = ShiftCtrlAlt (SDL.keyModifierLeftShift km || SDL.keyModifierRightShift km)
                                 (SDL.keyModifierLeftCtrl  km || SDL.keyModifierRightCtrl km)
                                 (SDL.keyModifierLeftAlt km || SDL.keyModifierRightAlt km || SDL.keyModifierAltGr km)
{-# INLINE getShftCtrlAlt #-}

isEnterKey :: SDL.Keycode -> Bool
isEnterKey keycode = keycode == SDL.KeycodeReturn || -- keycode == SDL.KeycodeReturn2 ||
                     keycode == SDL.KeycodeCaret || keycode == SDL.KeycodeKPEnter
{-# INLINE isEnterKey #-}

-- Нужна только для KeyWithModifiers-ев, по этому, не подходящие для них клавишы закомментированы
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
showbKeycode SDL. = ""
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

data KeyWithModifiers = KeyWithModifiers KeyModifiers SDL.Keycode
                    deriving (Data, Eq, Ord, Generic, Typeable)

instance TextShow KeyWithModifiers where
    showb (KeyWithModifiers km k) = showb km <> showbKeycode k
{-
instance Show KeyWithModifiers where
    show = toString . showb
-}
scaToKeyModifier :: ShiftCtrlAlt -> KeyModifiers
scaToKeyModifier ShiftCtrlAlt{isShift= False, isCtrl= True, isAlt= False  } = KeyCtrl
scaToKeyModifier ShiftCtrlAlt{isShift= True, isCtrl= False, isAlt= False } = KeyShift
scaToKeyModifier ShiftCtrlAlt{isShift= False, isCtrl=  False, isAlt=  True} = KeyAlt
scaToKeyModifier ShiftCtrlAlt{isShift= True, isCtrl=  True, isAlt=  False} = KeyCtrlShift
scaToKeyModifier ShiftCtrlAlt{isShift= False, isCtrl=  True, isAlt=  True} = KeyCtrlAlt
scaToKeyModifier ShiftCtrlAlt{isShift= True, isCtrl= False, isAlt=  True} = KeyShiftAlt
scaToKeyModifier _ = KeyNoModifiers
{-# INLINE scaToKeyModifier #-}

