module GUI.BaseLayer.Keyboard(
    ShiftCtrlAlt(..),getShftCtrlAlt,isEnterKey,showKeycode,KeyModifiers(..),KeyWithModifiers(..)
    ,scaToKeyModifier
    ) where

import qualified SDL
import Numeric
import Data.Char

data ShiftCtrlAlt = ShiftCtrlAlt { isShift :: Bool
                                 , isCtrl  :: Bool
                                 , isAlt   :: Bool
                                 }
                                 deriving (Eq,Ord)

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
showKeycode :: SDL.Keycode -> String
{-
showKeycode SDL.KeycodeSpace  = "' '"
showKeycode SDL.KeycodeBackspace  = "Backspace"
showKeycode SDL.KeycodeTab = "Tab"
showKeycode SDL.KeycodeInsert = "Ins"
showKeycode SDL.KeycodeHome = "Home"
showKeycode SDL.KeycodePageUp  = "PgUp"
showKeycode SDL.KeycodeDelete  = "Del"
showKeycode SDL.KeycodeEnd  = "End"
showKeycode SDL.KeycodePageDown  = "PgDn"
showKeycode SDL.KeycodeRight  = "Right"
showKeycode SDL.KeycodeLeft = "Left"
showKeycode SDL.KeycodeDown = "Down"
showKeycode SDL.KeycodeUp  = "Up"
showKeycode SDL. = ""
showKeycode SDL.KeycodeEscape = "Esc" -}
showKeycode k
--    | isEnterKey k = "Enter"
    | k>= SDL.KeycodeF1 && k<=SDL.KeycodeF12 = 'F': show
        (1 + SDL.unwrapKeycode k - SDL.unwrapKeycode SDL.KeycodeF1)
    | otherwise = case SDL.unwrapKeycode k of
                    key | key<128 -> let c= chr $ fromIntegral key in
                                     if isPrint c then [toUpper c] else showLitChar c ""
                        | otherwise -> showHex key ""

data KeyModifiers = KeyNoModifiers
                    | KeyCtrl
                    | KeyShift
                    | KeyAlt
                    | KeyCtrlShift
                    | KeyCtrlAlt
                    | KeyShiftAlt
                    deriving (Eq, Ord)

instance Show KeyModifiers where
    show KeyNoModifiers = ""
    show KeyCtrl = "Ctrl-"
    show KeyShift = "Shift-"
    show KeyAlt = "Alt-"
    show KeyCtrlShift = "Ctrl-Shift-"
    show KeyCtrlAlt = "Ctrl-Alt-"
    show KeyShiftAlt = "Shift-Alt-"


data KeyWithModifiers = KeyWithModifiers KeyModifiers SDL.Keycode
                    deriving (Eq, Ord)

instance Show KeyWithModifiers where
    show (KeyWithModifiers km k) = show km ++ showKeycode k

scaToKeyModifier :: ShiftCtrlAlt -> KeyModifiers
scaToKeyModifier ShiftCtrlAlt{isShift= False, isCtrl= True, isAlt= False  } = KeyCtrl
scaToKeyModifier ShiftCtrlAlt{isShift= True, isCtrl= False, isAlt= False } = KeyShift
scaToKeyModifier ShiftCtrlAlt{isShift= False, isCtrl=  False, isAlt=  True} = KeyAlt
scaToKeyModifier ShiftCtrlAlt{isShift= True, isCtrl=  True, isAlt=  False} = KeyCtrlShift
scaToKeyModifier ShiftCtrlAlt{isShift= False, isCtrl=  True, isAlt=  True} = KeyCtrlAlt
scaToKeyModifier ShiftCtrlAlt{isShift= True, isCtrl= False, isAlt=  True} = KeyShiftAlt
scaToKeyModifier _ = KeyNoModifiers
{-# INLINE scaToKeyModifier #-}

