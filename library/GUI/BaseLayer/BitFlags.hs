{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GUI.BaseLayer.BitFlags where

import Data.Bits
import Data.Word

type FlagsInternType = Word64

newtype Flags k = Flags {unFlags :: FlagsInternType}
                deriving (Eq, Read, Show)
                
instance Bits (Flags k) where
    (Flags x) .&. (Flags y) = Flags (x .&. y)
    (Flags x) .|. (Flags y) = Flags (x .|. y)
    xor (Flags x) (Flags y) = Flags (xor x y)
    complement (Flags x) = Flags (complement x)
    shift (Flags x) i = Flags (shift x i)
    rotate (Flags x) i = Flags (rotate x i)
    bit i = Flags (bit i)
    testBit (Flags x) = testBit x
    bitSizeMaybe (Flags x) = bitSizeMaybe x
    bitSize (Flags x) = let ~(Just r)=bitSizeMaybe x in r
    isSigned (Flags x) = isSigned x
    popCount (Flags x) = popCount x

