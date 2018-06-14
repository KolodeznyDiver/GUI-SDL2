-- |
-- Module:      Data.Vector.Unboxed.Utils
-- Copyright:   (c) 2017-2018 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Набор вспомогательных функций для работы с __/Unboxed/__ векторами.

module Data.Vector.Unboxed.Utils where

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

-- | Переставить соседние элементы
swapNeighb :: VUM.Unbox a => Int -> -- ^ Левый из переставляемых элементов
              VU.Vector a ->
              VU.Vector a
swapNeighb ix = VU.modify $ \v -> VUM.swap v ix $ ix+1
{-# INLINE swapNeighb #-}