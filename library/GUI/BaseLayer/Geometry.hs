{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
module GUI.BaseLayer.Geometry(
    Alignment(..),VAlign(..),HAlign(..),DirectionVH(..)
    ,xV2,yV2,pointOfRect,sizeOfRect,hvAlignToAlignment,getVAlign,getHAlign,vAlignToOff,hAlignToOff
    ,rectToBriefStr,directionLetter,rectAlign,toBound,isInRect,rectIntersection,getRectLB,getRectRT,getRectRB
    ,rectCenter,isEmptyRect,marginSize,rectShrinkByMargin,rectGrowByMargin,marginToLTRB,marginToLT,marginToBriefStr
    ,moveRect,moveRectTo,rectMove,shrinkRect,shrinkRect',mkDotLineVector,sizeReplaceIfNoPositive,sizeRestoreNegative
    ,mkDotRectVector
    ) where

import Data.Ix
import qualified SDL
import GUI.BaseLayer.Types
import SDL.Vect
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM

data Alignment = AlignLeftTop | AlignLeftCenter | AlignLeftBottom
               | AlignCenterTop | AlignCenter | AlignCenterBottom
               | AlignRightTop | AlignRightCenter | AlignRightBottom
               deriving (Eq, Show)

data VAlign = VTop | VCenter | VBottom
                 deriving (Eq, Show)

data HAlign = HLeft | HCenter | HRight
                 deriving (Eq, Show)

data DirectionVH = DirectionH | DirectionV
                 deriving (Eq, Show)

xV2 :: V2 a -> a
xV2 (V2 x _) = x
{-# INLINE xV2 #-}

yV2 :: V2 a -> a
yV2 (V2 _ y) = y
{-# INLINE yV2 #-}
{-
unP :: Point V2 a -> V2 a
unP (P x) = x
{-# INLINE unP #-} -}

pointOfRect:: SDL.Rectangle a -> Point V2 a
pointOfRect (SDL.Rectangle p _) = p
{-# INLINE pointOfRect #-}

sizeOfRect:: SDL.Rectangle a -> V2 a
sizeOfRect (SDL.Rectangle _ sz) = sz
{-# INLINE sizeOfRect #-}

hvAlignToAlignment :: HAlign -> VAlign -> Alignment
hvAlignToAlignment HLeft VTop = AlignLeftTop
hvAlignToAlignment HCenter VTop = AlignCenterTop
hvAlignToAlignment HRight VTop = AlignRightTop
hvAlignToAlignment HLeft VCenter = AlignLeftCenter
hvAlignToAlignment HCenter VCenter = AlignCenter
hvAlignToAlignment HRight VCenter = AlignRightCenter
hvAlignToAlignment HLeft VBottom = AlignLeftBottom
hvAlignToAlignment HCenter VBottom = AlignCenterBottom
hvAlignToAlignment HRight VBottom = AlignRightBottom
{-# INLINE hvAlignToAlignment #-}

getVAlign :: Alignment -> VAlign
getVAlign align | align == AlignLeftTop || align == AlignCenterTop || align == AlignRightTop = VTop
                | align == AlignLeftCenter || align == AlignCenter || align == AlignRightCenter = VCenter
                | otherwise = VBottom
{-# INLINE getVAlign #-}

getHAlign :: Alignment -> HAlign
getHAlign align | align == AlignLeftTop || align == AlignLeftCenter || align == AlignLeftBottom = HLeft
                | align == AlignCenterTop || align == AlignCenter || align == AlignCenterBottom = HCenter
                | otherwise = HRight
{-# INLINE getHAlign #-}

vAlignToOff :: (Ord a, Integral a) => VAlign -> a -> a
vAlignToOff VTop _ = 0
vAlignToOff VCenter delta = max 0 $ delta `div` 2
vAlignToOff _ delta = max 0 delta
{-# INLINE vAlignToOff #-}

hAlignToOff :: (Ord a, Integral a) => HAlign -> a -> a
hAlignToOff HLeft _ = 0
hAlignToOff HCenter delta = max 0 $ delta `div` 2
hAlignToOff _ delta = max 0 delta
{-# INLINE hAlignToOff #-}

rectToBriefStr :: Show a => SDL.Rectangle a -> String
rectToBriefStr (SDL.Rectangle (P (V2 x y)) (V2 w h)) = concat [show x,"  ",show y,"  ",show w,"  ",show h]

directionLetter :: DirectionVH -> Char
directionLetter DirectionV = 'v'
directionLetter _ = 'h'

rectAlign :: Integral a => Alignment -> V2 a -> SDL.Rectangle a -> SDL.Rectangle a
rectAlign align sz@(V2 wi hi) (SDL.Rectangle (P (V2 x y)) (V2 w h)) =
    let x' = x + hAlignToOff (getHAlign align) (w-wi)
        y' = y + vAlignToOff (getVAlign align) (h-hi)
    in SDL.Rectangle (P (V2 x' y')) sz
{-# INLINE rectAlign #-}

toBound:: Ord a => a -> a -> a -> a
toBound l h x = if x<l then l else if x>h then h else x
{-# INLINE toBound #-}

isInRect:: (Ix a, Num a) => SDL.Rectangle a -> Point V2 a  -> Bool
isInRect (SDL.Rectangle (P (V2 x0 y0)) (V2 w h)) (P (V2 x y)) = 
    inRange (x0,x0+w) x && inRange (y0,y0+h) y

rectIntersection :: (Ord a, Num a) => SDL.Rectangle a -> SDL.Rectangle a -> SDL.Rectangle a
rectIntersection (SDL.Rectangle (P (V2 x0 y0)) (V2 w0 h0)) (SDL.Rectangle (P (V2 x1 y1)) (V2 w1 h1)) =
    let l = max x0 x1
        t = max y0 y1
        r = min (x0 + w0) (x1 + w1)
        b = min (y0 + h0) (y1 + h1)
    in if (l>=r) || (t>=b) then SDL.Rectangle (P (V2 0 0)) (V2 0 0)
       else SDL.Rectangle (P (V2 l t)) (V2 (r - l) (b - t))

getRectLB :: Num a => SDL.Rectangle a -> SDL.Point V2 a
getRectLB (SDL.Rectangle (P (V2 x y)) (V2 _ h))  = P (V2 x (y+h))
{-# INLINE getRectLB #-}

getRectRT :: Num a => SDL.Rectangle a -> SDL.Point V2 a
getRectRT (SDL.Rectangle (P (V2 x y)) (V2 w _))  = P (V2 (x+w) y)
{-# INLINE getRectRT #-}

getRectRB :: Num a => SDL.Rectangle a -> SDL.Point V2 a
getRectRB (SDL.Rectangle p sz) = p .+^ sz
{-# INLINE getRectRB #-}

rectCenter :: Integral a => SDL.Rectangle a -> SDL.Point V2 a
rectCenter (SDL.Rectangle (P (V2 x y)) (V2 w h)) = P (V2 (x + w `div` 2) (y + h `div` 2))

isEmptyRect :: (Ord a, Num a) => SDL.Rectangle a -> Bool
isEmptyRect (SDL.Rectangle _ (V2 x y)) = x <= 0 || y <= 0
{-# INLINE isEmptyRect #-}

marginSize :: Num a => MarginLTRB a -> V2 a
marginSize MarginLTRB{..} = V2 (leftMargin + rightMargin) (topMargin + bottomMargin)
{-# INLINE marginSize #-}

rectShrinkByMargin :: (Ord a, Num a) => MarginLTRB a -> SDL.Rectangle a -> SDL.Rectangle a
rectShrinkByMargin MarginLTRB{..} (SDL.Rectangle (P (V2 x0 y0)) (V2 w h)) =
    let w' = if w <0 then w else max 0 (w - leftMargin - rightMargin)
        h' = if h <0 then h else max 0 (h - topMargin - bottomMargin)
    in SDL.Rectangle (P (V2 (if w <0 then x0 else x0+leftMargin)
                            (if h <0 then y0 else y0+topMargin))) (V2 w' h')

rectGrowByMargin :: (Ord a, Num a) => MarginLTRB a -> SDL.Rectangle a -> SDL.Rectangle a
rectGrowByMargin m = rectShrinkByMargin $ fmap negate m
{-# INLINE rectGrowByMargin #-}

marginToLTRB :: WidgetMargin -> GuiMargin
marginToLTRB WidgetMarginNone = MarginLTRB 0 0 0 0
marginToLTRB (WidgetMarginEvenly x) = MarginLTRB x x x x
marginToLTRB (WidgetMarginXY x y)   = MarginLTRB x y x y
marginToLTRB (WidgetMarginLTRB l t r b) = MarginLTRB l t r b
{-# INLINE marginToLTRB #-}

marginToLT :: GuiMargin -> V2 Coord
marginToLT MarginLTRB{..} = V2 leftMargin topMargin
{-# INLINE marginToLT #-}

marginToBriefStr :: GuiMargin -> String
marginToBriefStr MarginLTRB{..} = concat ["<",show leftMargin, "^", show topMargin,
                                        ">", show rightMargin, "v", show bottomMargin]

moveRect :: Num a => V2 a -> SDL.Rectangle a -> SDL.Rectangle a
moveRect (V2 dx dy) (SDL.Rectangle (P (V2 x0 y0)) sz) =
             SDL.Rectangle (P (V2 (x0+dx) (y0+dy))) sz
{-# INLINE moveRect #-}

moveRectTo :: SDL.Point V2 a -> SDL.Rectangle a -> SDL.Rectangle a
moveRectTo p (SDL.Rectangle _ sz) = SDL.Rectangle p sz
{-# INLINE moveRectTo #-}

rectMove :: Num a => SDL.Rectangle a -> V2 a -> SDL.Rectangle a
rectMove (SDL.Rectangle (P (V2 x0 y0)) sz) (V2 dx dy) =
             SDL.Rectangle (P (V2 (x0+dx) (y0+dy))) sz
{-# INLINE rectMove #-}

shrinkRect :: (Ord a, Num a) => V2 a -> SDL.Rectangle a -> SDL.Rectangle a
shrinkRect (V2 dx dy) (SDL.Rectangle (P (V2 x0 y0)) (V2 w h)) =
    SDL.Rectangle (P (V2 (x0+dx) (y0+dy))) (V2 (max 0 $ w-2*dx) (max 0 $ h-2*dy))

shrinkRect' :: (Ord a, Num a) => a -> SDL.Rectangle a -> SDL.Rectangle a
shrinkRect' dt = shrinkRect (V2 dt dt)
{-# INLINE shrinkRect' #-}

sizeReplaceIfNoPositive :: (Ord a, Num a) => V2 a -> V2 a -> V2 a
sizeReplaceIfNoPositive (V2 minW minH) (V2 w h) = V2 (f minW w) (f minH h)
    where f a b | b>0 = b
                | otherwise = a
{-# INLINE sizeReplaceIfNoPositive #-}

sizeRestoreNegative :: (Ord a, Num a) => V2 a -> V2 a -> V2 a
sizeRestoreNegative (V2 initW initH) (V2 w h) = V2 (f initW w) (f initH h)
    where f a b | a<0 = a
                | otherwise = b
{-# INLINE sizeRestoreNegative #-}

mkDotLineVector :: Coord -> GuiPoint -> GuiPoint -> V.Vector GuiPoint
mkDotLineVector step p0 p1 =
    let sz = p1 .-. p0
        step' = ((step *). signum) <$> sz
        stepF = fromIntegral step :: Double
        cnt = case (\v -> truncate $ abs $ fromIntegral v / stepF) <$> sz of
                V2 0 cntY -> cntY
                V2 cntX 0 -> cntX
                V2 cntX cntY -> min cntX cntY
    in V.create $ do
            v <- VM.new cnt
            let go i p | i == cnt = return v
                       | otherwise = VM.write v i p >> go (i+1) (p .+^ step')
            go 0 p0

mkDotRectVector :: Coord -> SDL.Rectangle Coord -> V.Vector GuiPoint
mkDotRectVector step (SDL.Rectangle p0 (V2 w h)) =
    let cornSpan = 3
        spanX = V2 cornSpan 0
        spanY = V2 0 cornSpan
        p = SDL.Rectangle p0 (V2 (w-1) (h-1))
        pRT = getRectRT p
        pLB = getRectLB p
        pRB = getRectRB p
    in V.concat [ mkDotLineVector step (p0 .+^ spanX) (pRT .-^ spanX)
                , mkDotLineVector step (pRT .+^ spanY) (pRB .-^ spanY)
                , mkDotLineVector step (pLB .+^ spanX) (pRB .-^ spanX)
                , mkDotLineVector step (p0 .+^ spanY) (pLB .-^ spanY)
                ]
{-
data BorderResizeState a = BorderNoResize
                         | BorderResizeLeft a
                         | BorderResizeTop a
                         | BorderResizeRight a
                         | BorderResizeBottom a
                         | BorderResizeLeftTop (V2 a)
                         | BorderResizeRightTop (V2 a)
                         | BorderResizeLeftBottom (V2 a)
                         | BorderResizeRightBottom (V2 a)
                       deriving (Eq,Show)


data IntersectState a  = IntersectNo
                       | IntersectLo a 
                       | IntersectHi a 
                       | IntersectIn a 
                       deriving (Eq,Show)

intersectState:: (Ord a, Num a) => a ->  a -> a -> a -> IntersectState a
intersectState _ l h x | not (inRange (l,h) x) = IntersectNo
intersectState brdrW l _ x | x < (l + brdrW) = IntersectLo $ x - l
intersectState brdrW _ h x | x > (h - brdrW) = IntersectHi $ x - h
intersectState _ l _ x = IntersectIn $ x - l

data RectIntersectState a = RectIntersectState (IntersectState a) (IntersectState a)
                       deriving (Eq,Show)
     
rectIntersectState:: (Ord a, Num a) => a -> SDL.Rectangle a -> Point V2 a  -> RectIntersectState a
rectIntersectState brdrW (SDL.Rectangle (P (V2 x0 y0)) (V2 w h)) (P (V2 x y)) = 
    let sx = intersectState brdrW x0 (x0+w) x
        sy = intersectState brdrW y0 (y0+h) y in
    if (sx == IntersectNo) || (sy == IntersectNo) then RectIntersectState IntersectNo IntersectNo
    else RectIntersectState sx sy

borderResizeStateFromRectIntersectState :: RectIntersectState a -> BorderResizeState a
borderResizeStateFromRectIntersectState (RectIntersectState (IntersectLo x) (IntersectLo y)) = BorderResizeLeftTop (V2 x y)
borderResizeStateFromRectIntersectState (RectIntersectState (IntersectLo x) (IntersectHi y)) = BorderResizeLeftBottom (V2 x y)
borderResizeStateFromRectIntersectState (RectIntersectState (IntersectLo x) (IntersectIn _)) = BorderResizeLeft x
borderResizeStateFromRectIntersectState (RectIntersectState (IntersectHi x) (IntersectLo y)) = BorderResizeRightTop (V2 x y)
borderResizeStateFromRectIntersectState (RectIntersectState (IntersectHi x) (IntersectHi y)) = BorderResizeRightBottom (V2 x y)
borderResizeStateFromRectIntersectState (RectIntersectState (IntersectHi x) (IntersectIn _)) = BorderResizeRight x
borderResizeStateFromRectIntersectState (RectIntersectState (IntersectIn _) (IntersectLo y)) = BorderResizeTop y
borderResizeStateFromRectIntersectState (RectIntersectState (IntersectIn _) (IntersectHi y)) = BorderResizeBottom y
borderResizeStateFromRectIntersectState _ = BorderNoResize

borderResizeState:: (Ord a, Num a) => a -> SDL.Rectangle a -> Point V2 a  -> BorderResizeState a
borderResizeState brdrW r = borderResizeStateFromRectIntersectState . rectIntersectState brdrW r
-}