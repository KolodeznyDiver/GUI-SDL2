-- |
-- Module:      Data.Vector.Utils
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <kolodeznydiver@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Набор вспомогательных функций для работы с векторами.

module Data.Vector.Utils where

import Control.Monad
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as VUM
import Control.Monad.Primitive

unsafeDelElemByIx :: Int -> V.Vector a -> V.Vector a 
unsafeDelElemByIx delIx v = let lastIx = V.length v - 1 in 
                            V.unsafeSlice 0 delIx v V.++ V.unsafeSlice (delIx+1) (lastIx - delIx) v

-- | 
delElem :: Eq a => a -> V.Vector a -> V.Vector a 
delElem a v = maybe v (`unsafeDelElemByIx` v) $ V.elemIndex a v 

swapWithLastByIx :: Int -> V.Vector a -> V.Vector a 
swapWithLastByIx ix v = let lastIx = V.length v - 1 in 
                        if ix>=0 && ix<lastIx then
                                  V.concat [V.unsafeSlice 0      ix v, 
                                           V.unsafeSlice lastIx 1  v, 
                                           V.unsafeSlice (ix+1) (lastIx - ix - 1) v,
                                           V.unsafeSlice ix     1  v]
                        else v

swapWithLast :: Eq a => a -> V.Vector a -> V.Vector a 
swapWithLast a v = maybe v (`swapWithLastByIx` v) $ V.elemIndex a v

moveLastToFirst :: V.Vector a -> V.Vector a
moveLastToFirst v = V.cons ( V.last v) $ V.unsafeSlice 0 (V.length v - 1) v

-- | Применяет модифицирует указанный диапазон вектора с помощью функции.
modifySlice :: (PrimMonad m, VUM.Unbox a) =>
    VUM.MVector (PrimState m) a -> -- ^ Вектор.
    Int -> -- ^ Начало диапазона.
    Int -> -- ^ Длина диапазона
    (a -> a) -> -- ^ Модифицирующая функция
    m ()
modifySlice v from len f = go from len
    where go i cnt = when (cnt>0)
                        (VUM.modify v f i >> go (i+1) (cnt-1))