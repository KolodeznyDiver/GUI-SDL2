{-# LANGUAGE  TemplateHaskell #-}
-- |
-- Module:      GUI.Widget.TH
-- Copyright:   (c) 2017-2018 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- GUI-виджеты часто можно сгруппировать по принципу вертикальный-горизонтальный.
-- Например, вертикальный лайаут - горизонтальный лайаут,
-- вертикальный скролл- или трекбар и горизонтальный.
--
-- Имеющиеся в этом модуле наборы TH функций помогают описывать таикие виджеты однократно,
-- выбирая разный набор предложенных функций мы меняем во время компиляции вертикальные координаты с горизонтальными,
-- а так же вертикальные и горизонтальные выравнивания.

module GUI.Widget.TH where

import Language.Haskell.TH
import GUI.BaseLayer.Depend1.Geometry
import qualified SDL
import SDL.Vect

data ParallelOrthogonallyExpQFns = ParallelOrthogonallyExpQFns  { parallelMkV2 :: ExpQ
                                                                , orthoMkV2  :: ExpQ
                                                                , parallelV2 :: ExpQ
                                                                , orthoV2 :: ExpQ
                                                                , parallelGetFst :: ExpQ
                                                                , parallelGetSnd :: ExpQ
                                                                , orthoGetFst :: ExpQ
                                                                , orthoGetSnd :: ExpQ
                                                                , parallelRect :: ExpQ
                                                                , orthoRect :: ExpQ
                                                                , parallelGetAlign :: ExpQ
                                                                , orthoGetAlign :: ExpQ
                                                                , parallelGetAlignOff :: ExpQ
                                                                , orthoGetAlignOff :: ExpQ
                                                                }

horizontalFns :: ParallelOrthogonallyExpQFns
horizontalFns = ParallelOrthogonallyExpQFns{
                 parallelMkV2 = [| (\a b -> V2 a b) |]
                ,orthoMkV2 = [| (\a b -> V2 b a) |]
                ,parallelV2 = [| (\(V2 a b) -> V2 a b) |]
                ,orthoV2 = [| (\(V2 a b) -> V2 b a) |]
                ,parallelGetFst = [| (\(V2 a _) -> a) |]
                ,parallelGetSnd = [| (\(V2 _ b) -> b) |]
                ,orthoGetFst = [| (\(V2 _ b) -> b) |]
                ,orthoGetSnd = [| (\(V2 a _) -> a) |]
                ,parallelRect = [| (\(SDL.Rectangle (P (V2 a b)) (V2 w h)) -> SDL.Rectangle (P (V2 a b)) (V2 w h)) |]
                ,orthoRect =  [| (\(SDL.Rectangle (P (V2 a b)) (V2 w h)) -> SDL.Rectangle (P (V2 b a)) (V2 h w)) |]
                ,parallelGetAlign = [| \a -> getHAlign a |]
                ,orthoGetAlign = [| \a -> getVAlign a |]
                ,parallelGetAlignOff = [| \a -> hAlignToOff a |]
                ,orthoGetAlignOff = [| \a -> vAlignToOff a |]
                }

verticalFns :: ParallelOrthogonallyExpQFns
verticalFns = let s=horizontalFns in
              ParallelOrthogonallyExpQFns{
                 parallelMkV2 = orthoMkV2 s
                ,orthoMkV2 = parallelMkV2 s
                ,parallelV2 = orthoV2 s
                ,orthoV2 = parallelV2 s
                ,parallelGetFst = orthoGetFst s
                ,parallelGetSnd = orthoGetSnd s
                ,orthoGetFst = parallelGetFst s
                ,orthoGetSnd = parallelGetSnd s
                ,parallelRect = orthoRect s
                ,orthoRect = parallelRect s
                ,parallelGetAlign = orthoGetAlign s
                ,orthoGetAlign = parallelGetAlign s
                ,parallelGetAlignOff = orthoGetAlignOff s
                ,orthoGetAlignOff = parallelGetAlignOff s
                              }

getParallelOrthogonallyExpQFns :: DirectionVH -> ParallelOrthogonallyExpQFns
getParallelOrthogonallyExpQFns DirectionH = horizontalFns
getParallelOrthogonallyExpQFns _          = verticalFns
{-# INLINE getParallelOrthogonallyExpQFns #-}