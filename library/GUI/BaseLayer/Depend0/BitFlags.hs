{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module:      GUI.BaseLayer.Depend0.BitFlags
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Тип с битовыми флагами для бинарных настроек. Тэг __k__ позволяет создавать несмешиваемые
-- наборы флагов.

module GUI.BaseLayer.Depend0.BitFlags where

import Data.Bits
import Data.Word

-- | Тип внутреннего хранения флагов. Один бит на флаг.
type FlagsInternType = Word64

-- | Тип с битовыми флагами для бинарных настроек. Тэг __k__ позволяет создавать несмешиваемые
-- наборы флагов.
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

