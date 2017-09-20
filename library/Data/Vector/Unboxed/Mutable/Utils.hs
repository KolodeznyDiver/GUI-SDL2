-- |
-- Module:      Data.Vector.Unboxed.Mutable.Utils
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Набор вспомогательных функций для работы с __/Unboxed.Mutable/__ векторами.

module Data.Vector.Unboxed.Mutable.Utils where

import Control.Monad
import qualified Data.Vector.Unboxed.Mutable as VUM
import Control.Monad.Primitive

-- | Mодифицирует указанный диапазон вектора с помощью функции.
modifySlice :: (PrimMonad m, VUM.Unbox a) =>
    VUM.MVector (PrimState m) a -> -- ^ Вектор.
    Int -> -- ^ Начало диапазона.
    Int -> -- ^ Длина диапазона
    (a -> a) -> -- ^ Модифицирующая функция
    m ()
modifySlice v from len f = go from len
    where go i cnt = when (cnt>0)
                        (VUM.modify v f i >> go (i+1) (cnt-1))
