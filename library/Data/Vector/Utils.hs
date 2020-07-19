-- |
-- Module:      Data.Vector.Utils
-- Copyright:   (c) 2017-2020 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Набор вспомогательных функций для работы с векторами.

module Data.Vector.Utils where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

unsafeDelElemByIx :: Int -> V.Vector a -> V.Vector a 
unsafeDelElemByIx delIx v = let lastIx = V.length v - 1 in 
                            V.unsafeSlice 0 delIx v V.++ V.unsafeSlice (delIx+1) (lastIx - delIx) v

-- | 
delElem :: Eq a => a -> V.Vector a -> V.Vector a 
delElem a v = maybe v (`unsafeDelElemByIx` v) $ V.elemIndex a v 

swapWithLastByIx :: Int -> V.Vector a -> V.Vector a 
swapWithLastByIx ix v = let lastIx = V.length v - 1 in
                        if ix>=0 && ix<lastIx then
                            V.modify (\v' -> VM.swap v' ix lastIx) v
{-
                                  V.concat [V.unsafeSlice 0      ix v,
                                           V.unsafeSlice lastIx 1  v, 
                                           V.unsafeSlice (ix+1) (lastIx - ix - 1) v,
                                           V.unsafeSlice ix     1  v]
-}
                        else v
swapWithLast :: Eq a => a -> V.Vector a -> V.Vector a
swapWithLast a v = maybe v (`swapWithLastByIx` v) $ V.elemIndex a v

-- | Переставить соседние элементы
swapNeighb :: Int -> -- ^ Левый из переставляемых элементов
              V.Vector a ->
              V.Vector a
swapNeighb ix = V.modify $ \v -> VM.swap v ix $ ix+1
{-# INLINE swapNeighb #-}

moveLastToFirst :: V.Vector a -> V.Vector a
moveLastToFirst v = V.cons ( V.last v) $ V.unsafeSlice 0 (V.length v - 1) v

-- | Вставить элемент в указанную позицию
insert :: Int -> a -> V.Vector a -> V.Vector a
insert pos n v = V.slice 0 pos v V.++ V.cons n (V.slice pos (V.length v - pos) v)
{-# INLINEABLE insert #-}
