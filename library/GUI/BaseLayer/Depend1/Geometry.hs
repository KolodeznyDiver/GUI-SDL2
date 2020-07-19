{-# LANGUAGE RecordWildCards #-}
-- |
-- Module:      GUI.BaseLayer.Depend1.Geometry
-- Copyright:   (c) 2017-2020 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Функции геометрических преобразований.

module GUI.BaseLayer.Depend1.Geometry(
    -- * Двухмерный вектор @V2 x y@.
    xV2,yV2,sizeReplaceIfNoPositive,sizeRestoreNegative,moveSegmentIntoSegment
    -- * Прямоугольник.
    ,getRectLT,getRectLB,getRectRT,getRectRB,rectCenter,sizeOfRect,isEmptyRect,isInRect
    ,rectIntersection,moveRect,moveRectTo,rectMove,moveRectIntoRect,shrinkRect,shrinkRect',shrinkRectNoLim
    ,rectFromCenterAndSz,rectToBriefStr
    -- * Выравнивание.
    ,Alignment(..),VAlign(..),HAlign(..)
    ,hvAlignToAlignment,getVAlign,getHAlign,vAlignToOff,hAlignToOff,rectAlign
    -- * Отступы (margin)
    ,marginSize,rectShrinkByMargin,rectGrowByMargin,marginToLTRB,marginToBriefStr
    -- * Генерация векторов точек.
    ,mkDotLineVector,mkDotRectVector
    -- * Прочее
    ,mulDiv,toBound,DirectionVH(..),directionLetter
    ) where

import Data.Ix
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import qualified SDL
import SDL.Vect
import GUI.BaseLayer.Depend0.Types

--------------------------- Двухмерный вектор -----------------

-- | Возвращает координату __/x/__ двухмерного вектора.
xV2 :: V2 a -> a
xV2 (V2 x _) = x
{-# INLINE xV2 #-}

-- | Возвращает координату __/y/__ двухмерного вектора.
yV2 :: V2 a -> a
yV2 (V2 _ y) = y
{-# INLINE yV2 #-}

-- | По каждой координате : если во втором аргументе координата \>0, в выходной вектор подставляется она,
-- иначе координата из первого аргумента.
sizeReplaceIfNoPositive :: (Ord a, Num a) => V2 a -> V2 a -> V2 a
sizeReplaceIfNoPositive (V2 minW minH) (V2 w h) = V2 (f minW w) (f minH h)
    where f a b | b>0 = b
                | otherwise = a
{-# INLINE sizeReplaceIfNoPositive #-}

-- | По каждой координате : если в первом аргументе координата \<0, в выходной вектор подставляется она,
-- иначе координата из второго аргумента.
sizeRestoreNegative :: (Ord a, Num a) => V2 a -> V2 a -> V2 a
sizeRestoreNegative (V2 initW initH) (V2 w h) = V2 (f initW w) (f initH h)
    where f a b | a<0 = a
                | otherwise = b
{-# INLINE sizeRestoreNegative #-}

----------------------------- Прямоугольник -----------------------------

-- | Возвращает левую верхнюю точку прямоугольника.
getRectLT:: SDL.Rectangle a -> Point V2 a
getRectLT (SDL.Rectangle p _) = p
{-# INLINE getRectLT #-}

-- | Возвращает левую нижнюю точку прямоугольника.
getRectLB :: Num a => SDL.Rectangle a -> SDL.Point V2 a
getRectLB (SDL.Rectangle (P (V2 x y)) (V2 _ h))  = P (V2 x (y+h))
{-# INLINE getRectLB #-}

-- | Возвращает правую верхнюю точку прямоугольника.
getRectRT :: Num a => SDL.Rectangle a -> SDL.Point V2 a
getRectRT (SDL.Rectangle (P (V2 x y)) (V2 w _))  = P (V2 (x+w) y)
{-# INLINE getRectRT #-}

-- | Возвращает правую нижнюю точку прямоугольника.
getRectRB :: Num a => SDL.Rectangle a -> SDL.Point V2 a
getRectRB (SDL.Rectangle p sz) = p .+^ sz
{-# INLINE getRectRB #-}

-- | Возвращает точку в центре прямоугольника.
rectCenter :: Integral a => SDL.Rectangle a -> SDL.Point V2 a
rectCenter (SDL.Rectangle (P (V2 x y)) (V2 w h)) = P (V2 (x + w `div` 2) (y + h `div` 2))

-- | Возвращает размер прямоугольника.
sizeOfRect:: SDL.Rectangle a -> V2 a
sizeOfRect (SDL.Rectangle _ sz) = sz
{-# INLINE sizeOfRect #-}

-- | Прямоугольник пуст если ширина или высота \<= 0.
isEmptyRect :: (Ord a, Num a) => SDL.Rectangle a -> Bool
isEmptyRect (SDL.Rectangle _ (V2 w h)) = w <= 0 || h <= 0
{-# INLINE isEmptyRect #-}

-- | Точка в границах прямоугольника ?
isInRect:: (Ix a, Num a) => SDL.Rectangle a -> Point V2 a  -> Bool
isInRect (SDL.Rectangle (P (V2 x0 y0)) (V2 w h)) (P (V2 x y)) =
    inRange (x0,x0+w) x && inRange (y0,y0+h) y

-- | Вычисляет прямоугольник являющийся пересечением прямоугольников, или прямоугольник со
-- всеми нулевыми координатами, если исходные прямоугольники не пересекаются.
rectIntersection :: (Ord a, Num a) => SDL.Rectangle a -> SDL.Rectangle a -> SDL.Rectangle a
rectIntersection (SDL.Rectangle (P (V2 x0 y0)) (V2 w0 h0)) (SDL.Rectangle (P (V2 x1 y1)) (V2 w1 h1)) =
    let l = max x0 x1
        t = max y0 y1
        r = min (x0 + w0) (x1 + w1)
        b = min (y0 + h0) (y1 + h1)
    in if (l>=r) || (t>=b) then SDL.Rectangle (P (V2 0 0)) (V2 0 0)
       else SDL.Rectangle (P (V2 l t)) (V2 (r - l) (b - t))

-- | Переместить прямоугольник на указанное расстояние.
moveRect :: Num a => V2 a -> SDL.Rectangle a -> SDL.Rectangle a
moveRect (V2 dx dy) (SDL.Rectangle (P (V2 x0 y0)) sz) =
             SDL.Rectangle (P (V2 (x0+dx) (y0+dy))) sz
{-# INLINE moveRect #-}

-- | Переместить прямоугольник в указанную точку (левый верхний край прямоугольника).
moveRectTo :: SDL.Point V2 a -> SDL.Rectangle a -> SDL.Rectangle a
moveRectTo p (SDL.Rectangle _ sz) = SDL.Rectangle p sz
{-# INLINE moveRectTo #-}

-- | Переместить прямоугольник на указанное расстояние. @moveRect@ с переставленными местами аргументами.
rectMove :: Num a => SDL.Rectangle a -> V2 a -> SDL.Rectangle a
rectMove r off = moveRect off r
{-# INLINE rectMove #-}

-- | Переместить второй прямоугольник на минимальное расстояние, но так, что бы он оказался внутри
--   первого прямоугольника.
moveRectIntoRect :: (Ord a, Num a) => SDL.Rectangle a -> SDL.Rectangle a -> SDL.Rectangle a
moveRectIntoRect (SDL.Rectangle (P (V2 x0 y0)) (V2 w0 h0))
                 (SDL.Rectangle (P (V2 x y)) sz@(V2 w h)) =
    SDL.Rectangle (P (V2 (moveSegmentIntoSegment x0 w0 x w)
                         (moveSegmentIntoSegment y0 h0 y h))) sz
{-# INLINEABLE moveRectIntoRect #-}

-- | Создать прямоугольник из сентральной точки и размера.
rectFromCenterAndSz :: Integral a => SDL.Point V2 a -> V2 a -> SDL.Rectangle a
rectFromCenterAndSz p sz = SDL.Rectangle ( p .-^ ((`div` 2) <$> sz)) sz
{-# INLINE rectFromCenterAndSz #-}


-- | Уменьшить прямоугольник на указанные значения по координатам сохраняя его центр неизменным.
-- Если значения отрицательные - прямоугольник увеличивается.
-- Размеры в любом случае не окажутся меньше нуля.
shrinkRect :: (Ord a, Num a) => V2 a -> SDL.Rectangle a -> SDL.Rectangle a
shrinkRect (V2 dx dy) (SDL.Rectangle (P (V2 x0 y0)) (V2 w h)) =
    SDL.Rectangle (P (V2 (x0+dx) (y0+dy))) (V2 (max 0 $ w-2*dx) (max 0 $ h-2*dy))

-- | Как @shrinkRect@, но расстояния сжатия по __/x/__ и __/y/__ совпадают.
shrinkRect' :: (Ord a, Num a) => a -> SDL.Rectangle a -> SDL.Rectangle a
shrinkRect' dt = shrinkRect (V2 dt dt)
{-# INLINE shrinkRect' #-}

-- | Уменьшить прямоугольник на указанные значения по координатам сохраняя его центр неизменным.
-- Если значения отрицательные - прямоугольник увеличивается.
-- Размеры не контролируются и могут быть любыми.
shrinkRectNoLim :: (Ord a, Num a) => V2 a -> SDL.Rectangle a -> SDL.Rectangle a
shrinkRectNoLim (V2 dx dy) (SDL.Rectangle (P (V2 x0 y0)) (V2 w h)) =
    SDL.Rectangle (P (V2 (x0+dx) (y0+dy))) (V2 (w-2*dx) (h-2*dy))

-- | Отладочная функция преобразования координат прямоугольника в строку в сокращённом виде по сравнению с
-- @show@ для 'SDL.Rectangle'.
rectToBriefStr :: Show a => SDL.Rectangle a -> String
rectToBriefStr (SDL.Rectangle (P (V2 x y)) (V2 w h)) = concat [show x,"  ",show y,"  ",show w,"  ",show h]

---------------------------- Выравнивание ---------------------------------------

-- | 2D выравнивание.
data Alignment = AlignLeftTop | AlignLeftCenter | AlignLeftBottom
               | AlignCenterTop | AlignCenter | AlignCenterBottom
               | AlignRightTop | AlignRightCenter | AlignRightBottom
               deriving (Eq, Show)

-- | Выравнивание по вертикали.
data VAlign = VTop | VCenter | VBottom
                 deriving (Eq, Show)

-- | Выравнивание по горизонтали.
data HAlign = HLeft | HCenter | HRight
                 deriving (Eq, Show)

-- | Из горизонтального и вертикального выравнивания сделать 2D выравнивание.
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

-- | Из 2D выравнивания выделить вертикальное выравнивание.
getVAlign :: Alignment -> VAlign
getVAlign align | align == AlignLeftTop || align == AlignCenterTop || align == AlignRightTop = VTop
                | align == AlignLeftCenter || align == AlignCenter || align == AlignRightCenter = VCenter
                | otherwise = VBottom
{-# INLINE getVAlign #-}

-- | Из 2D выравнивания выделить горизонтальное выравнивание.
getHAlign :: Alignment -> HAlign
getHAlign align | align == AlignLeftTop || align == AlignLeftCenter || align == AlignLeftBottom = HLeft
                | align == AlignCenterTop || align == AlignCenter || align == AlignCenterBottom = HCenter
                | otherwise = HRight
{-# INLINE getHAlign #-}

-- | Из вертикального выравнивания и разницы вертикальных размеров между внешним и внутренним
--  прямоугольниками рассчитать вертикальное смещение внутреннего прямоугольника относитьно внешнего.
vAlignToOff :: (Ord a, Integral a) => VAlign -> a -> a
vAlignToOff VTop _ = 0
vAlignToOff VCenter delta = max 0 $ delta `div` 2
vAlignToOff _ delta = max 0 delta
{-# INLINE vAlignToOff #-}

-- | Из горизонтального выравнивания и разницы горизонтальных размеров между внешним и внутренним
--  прямоугольниками рассчитать горизонтальное смещение внутреннего прямоугольника относитьно внешнего.
hAlignToOff :: (Ord a, Integral a) => HAlign -> a -> a
hAlignToOff HLeft _ = 0
hAlignToOff HCenter delta = max 0 $ delta `div` 2
hAlignToOff _ delta = max 0 delta
{-# INLINE hAlignToOff #-}

-- | Рассчитать координаты выравниваемого внутреннего прямоугольника по заданному выравниванию,
-- размеру внутреннего  прямоугольника и координатам внешнего, относительно которого происходит выравнивание.
rectAlign :: Integral a => Alignment -> V2 a -> SDL.Rectangle a -> SDL.Rectangle a
rectAlign align sz@(V2 wi hi) (SDL.Rectangle (P (V2 x y)) (V2 w h)) =
    let x' = x + hAlignToOff (getHAlign align) (w-wi)
        y' = y + vAlignToOff (getVAlign align) (h-hi)
    in SDL.Rectangle (P (V2 x' y')) sz
{-# INLINE rectAlign #-}

----------------------- Отступы (margin) ---------------------

-- | Просуммировать покоординатно отступы из 'MarginLTRB' и вернуть в виде 'V2'.
marginSize :: Num a => MarginLTRB a -> V2 a
marginSize MarginLTRB{..} = V2 (leftMargin + rightMargin) (topMargin + bottomMargin)
{-# INLINE marginSize #-}

-- | Уменьшить прямоугольник - сдвинуть стороны к центру, на величины margin заданные в 'MarginLTRB'.
-- Если исходный размер прямоугольника по какой либо координате \<0, - прямоугольник не меняется по этой координате.
rectShrinkByMargin :: (Ord a, Num a) => MarginLTRB a -> SDL.Rectangle a -> SDL.Rectangle a
rectShrinkByMargin MarginLTRB{..} (SDL.Rectangle (P (V2 x0 y0)) (V2 w h)) =
    let w' = if w <0 then w else max 0 (w - leftMargin - rightMargin)
        h' = if h <0 then h else max 0 (h - topMargin - bottomMargin)
    in SDL.Rectangle (P (V2 (if w <0 then x0 else x0+leftMargin)
                            (if h <0 then y0 else y0+topMargin))) (V2 w' h')

-- | Увеличить прямоугольник - расдвинуть стороны от центра, на величины margin заданные в 'MarginLTRB'.
-- Если исходный размер прямоугольника по какой либо координате \<0, - прямоугольник не меняется по этой координате.
rectGrowByMargin :: (Ord a, Num a) => MarginLTRB a -> SDL.Rectangle a -> SDL.Rectangle a
rectGrowByMargin m = rectShrinkByMargin $ fmap negate m
{-# INLINE rectGrowByMargin #-}

-- | Преобразование типов описания отступов из 'WidgetMargin' в 'GuiMargin'
marginToLTRB :: WidgetMargin -> GuiMargin
marginToLTRB WidgetMarginNone = MarginLTRB 0 0 0 0
marginToLTRB (WidgetMarginEvenly x) = MarginLTRB x x x x
marginToLTRB (WidgetMarginXY x y)   = MarginLTRB x y x y
marginToLTRB (WidgetMarginLTRB l t r b) = MarginLTRB l t r b
{-# INLINE marginToLTRB #-}


-- | Отладочная функция преобразования 'GuiMargin' в строку в сокращённом виде по сравнению с
-- @show@ для 'GuiMargin'.
marginToBriefStr :: GuiMargin -> String
marginToBriefStr MarginLTRB{..} = concat ["<",show leftMargin, "^", show topMargin,
                                        ">", show rightMargin, "v", show bottomMargin]

-------------------------- Генерация векторов точек --------------------------

-- | Сгенерировать 'V.Vector' точек лежащих на прямой с заданным шагом.
-- Если линия не строго вертикальна или горизонтальна, расчёт весьма приблизителен.
mkDotLineVector :: Coord    -> -- ^ Шаг, в пикселях.
                   GuiPoint -> -- ^ Точка начала линии.
                   GuiPoint -> -- ^ Точка окончания линии.
                   V.Vector GuiPoint
mkDotLineVector step p0 p1 =
    let sz = p1 .-. p0
        step' = ((step *). signum) <$> sz
        stepF = fromIntegral step :: Double
        cnt = case (\v -> truncate $ abs $ fromIntegral v / stepF) <$> sz of
                V2 0 cntY -> cntY
                V2 cntX 0 -> cntX
                V2 cntX cntY -> max 1 (min cntX cntY)
    in V.create $ do
            v <- VM.new cnt
            let go i p | i == cnt = return v
                       | otherwise = VM.write v i p >> go (i+1) (p .+^ step')
            go 0 p0

-- | Сгенерировать 'V.Vector' точек формирующие прямоугольник.
mkDotRectVector :: Coord -> -- ^ Шаг, в пикселях.
                   SDL.Rectangle Coord -> -- ^ Координаты прямоугольника.
                   V.Vector GuiPoint
mkDotRectVector step (SDL.Rectangle p0 (V2 w h)) =
    let cornSpan = 0
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

--------------------------- Прочее --------------------------

-- | Целочисленное i*j/k
mulDiv :: Integral a => a -> -- ^ i
                        a -> -- ^ j
                        a -> -- ^ k
                        a
mulDiv a b c = fromInteger $ toInteger a * toInteger b `div` toInteger c
{-# INLINE mulDiv #-}

-- | Приведение значения к диапазону. Если значение выходит за какую то границу диапазона, то
-- возвращается значение этой границы, иначе исходное значение.
toBound:: Ord a => a -> -- ^ Левая граница.
                   a -> -- ^ Правая граница.
                   a -> -- ^ значение.
                   a
toBound l h x = if x<l then l else if x>h then h else x
{-# INLINE toBound #-}

-- | Направление : горизонтальное или вертикальное.
data DirectionVH = DirectionH | DirectionV
                 deriving (Eq, Show)

-- | Возвращает букву означающую направление : 'v' или 'h'
directionLetter :: DirectionVH -> Char
directionLetter DirectionV = 'v'
directionLetter _ = 'h'

-- | Переместить второй отрезок на минимальное расстояние, но так, что бы он оказался внутри
--   первого отрезка.
moveSegmentIntoSegment :: (Ord a, Num a) => a -> -- ^ Начало объемлющего отрезка
                                            a -> -- ^ Его длина.
                                            a -> -- ^ Начало перемещаемого отрезка
                                            a -> -- ^ Его длина.
                                            a    -- ^ Новое начало отрезка
moveSegmentIntoSegment x0 d0 x d | (x+d) > (x0+d0) = x0+d0-d
                                 | x < x0 = x0
                                 | otherwise = x
{-# INLINEABLE moveSegmentIntoSegment #-}

