{-# LANGUAGE RecordWildCards #-}
-- |
-- Module:      GUI.Utils.TextWrap
-- Copyright:   (c) 2017-2018 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Модуль поддержки универсального текста, как с переносом строк, так и нет.

module GUI.Utils.TextWrap(
    -- * 'PreparedWrapText'
    -- Более простой тип, требующий соблюдения одинакового шрифта при поготовки текста и при отрисовки.
    -- Кроме того, не предусматривает изменение текста.
    PreparedWrapText,prepareWrapText,getHeightOfPreparedWrapText,drawSplitPreparedText
    ,getLeftOfPreparedWrapText,textWrapModeToMbBkColor
    -- * 'PreparedText'.
    -- Тип включает в себя 'PreparedWrapText', ссылку на использованный шрифт и исходные параметры
    -- подкготовки шрифта. Более громоздок но более универсален.
    ,TextWrapMode(..),DrawTextDef(..),PreparedText(..)
    ,prepareText',prepareTextGui,prepareText,drawPreparedText
                        ) where

import Control.Monad.IO.Class
--import Control.Monad.Trans.Class
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Maybe
import Data.Default
import qualified SDL
import SDL.Vect
import SDL.Font (Font)
import GUI
import qualified GUI.BaseLayer.Primitives as P
import GUI.Utils.Wrap

-- | Разобранный для отрисовки (возможно) на нескольких строках текст.
data PreparedWrapText = PreparedWrapText
        -- | Текст разбитый на не непробельные последовательности символов (фрагменты).
        (V.Vector T.Text)
        -- | Координаты левых верхних точек отрисовки фрагментов.
        -- Для отрисовки достаточно первых двух полей.
        (VU.Vector GuiPoint)
        -- | Левая граница текста. Вначале (до выравнивания) равна 0. Потом увеличивается вместе с координатами всех точек.
        Coord
        -- | Высота всего текста. По вектору координат точно рассчитать нельзя, нужен ещё междустрочный интервал.
        Coord
        deriving (Show, Eq)

-- | Строит разобранный для отрисовки (возможно на нескольких строках) текст, который можно отрисовать
-- с помощью @drawSplitPreparedText@.
prepareWrapText :: MonadIO m =>
    -- | Выравнивания. 'HAlign' используется уже в @rowWrapping@ Для каждой строки.
    -- 'VAlign' применяется перед самым возвратом, используя функцию @moveWrapText@.
    Alignment ->
    -- | Прямоугольник в котором нужно выровнять текст.
    GuiRect ->
    -- | Черезстрочное расстояние.
    Double ->
    -- | Шрифт.
    Font ->
    -- | Исходный текст.
    T.Text ->
    m PreparedWrapText
prepareWrapText align r@(SDL.Rectangle (P (V2 x0 _)) (V2 width _)) lineSpacing fnt str =
    let wrds = splitToVector $ T.dropWhile (`elem` " \n") str
        cWords = V.length wrds in
    if cWords == 0 then return $ PreparedWrapText V.empty VU.empty x0 0
    else do
        szS <- VU.generateM cWords ((\s -> if T.head s =='\n' then return $ V2 maxBound 0 else P.textSize fnt s).
                    (wrds V.!) )
        (V2 spaceWidth spaceHeight) <- P.textSize fnt $ T.singleton ' '
--        liftIO $ putStrLn $ concat ["textSplitPrepare  width=", show width, "   szS=",show szS]
        let vv' = rowWrapping (getHAlign align) width spaceWidth szS
            deltaLine maxH = ceiling (lineSpacing * fromIntegral maxH)
            maxHeight = deltaLine $ V.maximum $ V.map snd vv'
            delEmptyLn out x =  let (l,rest) = V.splitAt 1 x in
                                if V.null l then out
                                else let (_,y) = V.head l in
                                     if y == 0 then let (i,e) = V.splitAt (V.length out - 1) out in
                                                    if V.null e then delEmptyLn out rest
                                                    else let (v,h) = V.head e in
                                                         delEmptyLn (V.snoc i (v,h+maxHeight)) rest
                                     else delEmptyLn (out V.++ l) rest
            vv = delEmptyLn V.empty vv'
            cElem = V.sum $ V.map (VU.length . fst) vv
--        liftIO $ putStrLn $ concat ["textSplitPrepare vv=", show vv, " lineSpacing=", show lineSpacing,
--           "  dY=", show (truncate ((1+lineSpacing) * (fromIntegral $ snd $ V.head vv))) ]
        let offsets = VU.create $ do
                        v <- VUM.new cElem
                        let go y i m | Just (vRow,maxH) <- vv V.!? i =
                                        let byRow j n | Just x <- vRow VU.!? j = do
                                                                VUM.write v n (V2 x y)
                                                                byRow (j+1) (n+1)
                                                      | otherwise = return n
                                        in byRow 0 m >>= go (y + deltaLine maxH) (i + 1)
                                     | otherwise = return v
                        go 0 0 0
            (V2 _ maxY) = VU.last offsets
            prepH = maxY + spaceHeight -- maxHeight -- snd (V.last vv)
--            offY = vAlignToOff (getVAlign align) $ height - prepH
--            pLT = p0 .+^ (V2 0 offY)
--        liftIO $ putStrLn $ concat ["textSplitPrepare offsets=", show offsets]
        return $ moveWrapText (getVAlign align) r $ PreparedWrapText
            (V.filter (T.singleton '\n' /=) wrds) (VU.map P offsets) 0 prepH
  where splitToVector = V.unfoldr $ \b -> let t = T.dropWhile (' '==) b in
                                          case T.uncons t of
                                            Just ('\t',rest) -> Just (T.pack "   ",rest)
                                            Just ('\n',rest) -> Just (T.singleton '\n',rest)
                                            Just _ -> let a@(l,_) = T.span (`notElem` " \t\n") t in
                                                      if T.null l then Nothing else Just a
                                            _ -> Nothing

-- V.unfoldr :: (b -> Maybe (a, b)) -> b -> Vector a
-- T.uncons :: Text -> Maybe (Char, Text)

-- | Выровнять 'PreparedWrapText' вертикально в заданном прямоугольнике, а по __/x/__ просто перенести в
-- область начала прямоугольника.
moveWrapText :: VAlign -> GuiRect -> PreparedWrapText -> PreparedWrapText
moveWrapText align (SDL.Rectangle (P (V2 newX newY)) (V2 _ height)) pwt@(PreparedWrapText vt vp oldX prepH)
    | VU.null vp = pwt
    | otherwise = let (P (V2 _ top)) = VU.head vp
                      off= V2 (newX - oldX) (newY + vAlignToOff align (height - prepH) - top) in
                  PreparedWrapText vt (VU.map ( .+^ off) vp) newX prepH

-- | Возвращает левую границу текстовой области.
getLeftOfPreparedWrapText :: PreparedWrapText -> Coord
getLeftOfPreparedWrapText (PreparedWrapText _ _ l _) = l
{-# INLINE getLeftOfPreparedWrapText #-}

-- | Возвращает высоту текстовой области.
getHeightOfPreparedWrapText :: PreparedWrapText -> Coord
getHeightOfPreparedWrapText (PreparedWrapText _ _ _ h) = h
{-# INLINE getHeightOfPreparedWrapText #-}

-- | Рисует подготовленный текст указанным шрифтом, цветом, и, возможно, цветом фона.
drawSplitPreparedText :: MonadIO m =>
    -- | Поготовленный для вывода текст
    PreparedWrapText ->
    -- | Шрифт, который должен быть тем же что и использовался при создании 'PreparedWrapText'
    Font ->
    -- | Цвет текста.
    GuiColor ->
    -- | Или цвет фона для более быстрой отрисовки, или Nothing - для медленной отрисовки по прозрачному фону.
    -- Примечание: если межстрочный интервал был \<1, то нельзя устанавливать непрозрачный фон - он будет
    -- затирать части соседних строк.
    Maybe GuiColor ->
    Canvas m ()
drawSplitPreparedText (PreparedWrapText vT vP _ _) fnt color mbBkColor =
    let f = case mbBkColor of
                Just bc -> drawTextOpaque fnt color bc
                _ -> drawText fnt color
    in V.imapM_ (\i -> f (vP VU.! i)) vT
{-# INLINE drawSplitPreparedText #-}

-- | Вспомогательная функция, которая выбирает прозрачный или не прозрачный фон использовать для отрисовки.
textWrapModeToMbBkColor :: TextWrapMode -> Skin -> GuiColor -> Maybe GuiColor
textWrapModeToMbBkColor TextNoWrap{} _ c = Just c
textWrapModeToMbBkColor TextWrap{textAreaLineSpacing=s} skin c
    | fromMaybe (formTextLineSpacing skin) s < 1 = Nothing
    | otherwise = Just c
{-# INLINE textWrapModeToMbBkColor #-}

----------- * 'PreparedText'.

-- | Описание способа вывода текста.
data TextWrapMode =
        -- | Без переноса строк
        TextNoWrap {
            -- | До скольки можно увеличить ширину заданного первоначально не в этом типе) прямоугольника.
            textAreaMaxWidth :: Coord
                   } |
        -- | С переносом строк
        TextWrap {
            -- | Максимально допустимая высота текста. (Минимальная задаётся прямоугольником не в этом типе).
            textAreaMaxHeight :: Coord,
            -- | Междустрочный интервал. Если Nothing, то взять из 'Skin'.
            textAreaLineSpacing :: Maybe Double
                 }
        deriving (Show, Eq)

instance Default TextWrapMode where
    def = TextNoWrap 0

-- | Подробное описание текста со способом вывода текста. И с переносом на другие строки, и без.
data DrawTextDef = DrawTextDef  {
    -- | Прямоугольник в котором выполняется выравнивание текста. Может расширяться до указанных пределов если
    -- они заданы в 'TextWrapMode'.
      drawTextRect :: GuiRect
    -- | 'TextWrapMode' см. выше.
    , drawTextWrap :: TextWrapMode
    -- | Текстовый ключ шрифта.
    , drawTextFontKey :: T.Text
    -- | Выравнивание.
    , drawTextAlignment :: Alignment
    -- | Собственно, сам выводимый текст.
    , drawTextText :: T.Text
                                }
                                deriving (Show, Eq)

-- | В таком формате текст хранится после разбора.
-- Тип включает в себя 'PreparedWrapText', ссылку на использованный шрифт и исходные параметры
-- подкготовки шрифта. Более громоздок но более универсален.
data PreparedText = PreparedText    {
    -- | 'PreparedWrapText' см. ранее.
      preparedText :: PreparedWrapText
    -- | Исходные параметры, по которым препарировался текст
    -- | (Кроме поля @drawTextRect@, которое может быть расширено, см. 'TextWrapMode'.)
    , preparedTextDef :: DrawTextDef
    -- | Шрифт
    , preparedTextFont :: Font
                                    }
                                    deriving (Show)

-- | Препарировать текст используя 'Skin' и 'Font'. Поле 'drawTextFontKey' в 'DrawTextDef' не исп-ся.
prepareText' :: MonadIO m => Skin -> Font -> DrawTextDef -> m PreparedText
prepareText' skin fnt d@DrawTextDef{..} = do
    let (SDL.Rectangle p' (V2 w' h')) = drawTextRect
    case drawTextWrap of
      TextNoWrap{..} -> do
        txSz@(V2 txW txH) <- P.textSize fnt $ drawTextText
        let (SDL.Rectangle p0@(P (V2 x _)) _) = rectAlign drawTextAlignment txSz drawTextRect
            extendedW = if txW > w' then min txW $ max textAreaMaxWidth w' else w'
            r = SDL.Rectangle p' (V2 extendedW (max txH h'))
{-        liftIO $ putStrLn $ concat ["prepareText  \"", T.unpack drawTextText, "\"  txSz=", show txSz,
            " drawTextRect=", rectToBriefStr drawTextRect,
            "  extendedW=", show extendedW,
            "  r0=", rectToBriefStr r0,"  r=", rectToBriefStr r] -}
        return $ PreparedText (PreparedWrapText (V.singleton drawTextText) (VU.singleton p0) x txH)
                    d{drawTextRect=r} fnt
      TextWrap {..} -> do
        t@(PreparedWrapText _ _ _ prepH) <- prepareWrapText drawTextAlignment drawTextRect
            (fromMaybe (formTextLineSpacing skin) textAreaLineSpacing) fnt drawTextText
        let h2 = if (prepH > h') && (textAreaMaxHeight > 0) then min prepH textAreaMaxHeight else h'
        return $ PreparedText t d{drawTextRect=SDL.Rectangle p' (V2 w' h2)} fnt

-- | Препарировать текст используя 'Gui'.
prepareTextGui :: MonadIO m => Gui -> DrawTextDef -> m PreparedText
prepareTextGui gui d@DrawTextDef{..} = do
    skin <- guiGetSkin gui
    rm <- guiGetResourceManager gui
    fnt <- rmGetFont rm drawTextFontKey
    prepareText' skin fnt d

-- | Препарировать текст используя текущий виджет (виджет нужен только для получения шрифта и 'Skin').
prepareText :: MonadIO m => Widget -> DrawTextDef -> m PreparedText
prepareText widget d = getGuiFromWidget widget >>= (`prepareTextGui` d)
{-# INLINE prepareText #-}

-- | Вывод препарированного текста. В отличии от @drawSplitPreparedText@ используется другой
-- тип представления подготовленного тексата в который предыдущий тип 'PreparedWrapText' входит как одно из полей.
-- Таким образом поддерживается большая косистентность, шрифт будет выбран именно тот что использован при
-- подготовке текста.
drawPreparedText :: MonadIO m =>
    -- | Подготовленный текст.
    PreparedText ->
    -- | Цвет текста.
    GuiColor ->
    -- | Или цвет фона для более быстрой отрисовки, или Nothing - для медленной отрисовки по прозрачному фону.
    -- Примечание: если межстрочный интервал был \<1, то нельзя устанавливать непрозрачный фон - он будет
    -- затирать части соседних строк.
    Maybe GuiColor ->
    Canvas m ()
drawPreparedText PreparedText{..} color mbBkColor =
    withClipRect (drawTextRect preparedTextDef) $ drawSplitPreparedText preparedText preparedTextFont color mbBkColor
{-# INLINE drawPreparedText #-}
