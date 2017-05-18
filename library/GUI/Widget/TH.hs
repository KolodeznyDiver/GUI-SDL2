{-# LANGUAGE  TemplateHaskell #-}
module GUI.Widget.TH where

import Language.Haskell.TH
import GUI.BaseLayer.Geometry
import qualified SDL
--import GUI.BaseLayer.Types
import SDL.Vect

data ParallelOrthogonallyExpQFns = ParallelOrthogonallyExpQFns  { parallelMkV2 :: ExpQ
                                                                , orthoMkV2  :: ExpQ
                                                                , parallelV2 :: ExpQ
                                                                , orthoV2 :: ExpQ
--                                                                , parallelGetFromP :: ExpQ
--                                                                , orthoGetFromP :: ExpQ
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
--                ,parallelGetFromP = [| (\(P (V2 a b)) -> V2 a b) |]
--                ,orthoGetFromP = [| (\(P (V2 a b)) -> V2 b a) |]
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
--                ,parallelGetFromP = orthoGetFromP s
--                ,orthoGetFromP = parallelGetFromP s
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