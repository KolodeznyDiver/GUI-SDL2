{- # LANGUAGE PatternSynonyms # -}
{- # LANGUAGE RankNTypes # -}
{-# LANGUAGE RecordWildCards #-}
{- # LANGUAGE PatternGuards # -}

-- |
-- Module:      GUI.Utils.TandTItem
-- Copyright:   (c) 2017-2018 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- TandTItem - (T)exture and (T)ext item

module GUI.Utils.TandTItem(
    -- * Подготовленные для отображения данные элемента состоящего из текстуры и текста.
    TandTItem
    -- Подготовка 'TandTItem' для отображения в дальнейшем.
    ,prepare
    -- Обновление 'TandTItem' при изменении размера.
    ,update
    -- Отрисовка элемента.
    ,draw
    -- Актуальные координаты элемента.
    ,getRect
    -- Актуальный размер области занимаемой элементом.
    ,getSize
    -- Находится ли указанная точка в границах отображения текстуры.
    ,inPictRect
    ) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified SDL
import SDL.Vect
import SDL.Font (Font)
import GUI
import qualified GUI.BaseLayer.Primitives as P
import GUI.Utils.TextWrap
--import TextShow

-- | Подготовленные для отображения данные элемента состоящего из текстуры и текста.
data TandTItem = TandTItem {
         ttRect :: GuiRect -- ^ Весь элемент.
        ,ttPictRect :: GuiRect -- ^ Область отображения картинки в координатах поля.
        ,ttTexture   :: SDL.Texture -- ^ текстура.
--        ,ttTextLMarg :: Coord -- ^ Левое смещение от границы поля до области вывода текста.
        ,ttPreparedText :: PreparedText -- ^ Текст, подготовленный для вывода.
--        ,ttTextRect :: GuiRect -- for debug
                           }

-- | Подготовка 'TandTItem' для отображения в дальнейшем.
prepare :: MonadIO m =>
            Widget -> -- Skin ->
            GuiRect -> -- ^ Весь элемент.
            SDL.Texture ->
            GuiSize -> -- ^ размер области отображения картинки.
            TextWrapMode -> -- ^ Описание способа вывода текста.
            Font ->
            T.Text ->
            m TandTItem
prepare widget srcR@(SDL.Rectangle srcP (V2 srcW srcH))
        texture txtrSz@(V2 txtrW txtrH) wm fnt txt = do
    skin <- getSkinFromWidget widget

    V2 _ textH <- P.textSize fnt txt
    let (SDL.Rectangle (P (V2 inX inY)) (V2 inW inH)) = shrinkRectNoLim (V2 MinInsideSpaceX MinInsideSpaceY) srcR
        txtrR = SDL.Rectangle (P (V2 inX (inY + (max 0 (textH - txtrH)) `div` 2))) txtrSz
        txtLeftMarg = txtrW + MinInsideSpaceX
        txtR@(SDL.Rectangle _ (V2 txtW txtH)) = SDL.Rectangle (P (V2 (inX+txtLeftMarg) inY)) (V2 (inW-txtLeftMarg) inH)
    prepTxt@PreparedText{preparedTextDef=DrawTextDef{drawTextRect=SDL.Rectangle _ (V2 w h)}}
        <- prepareText' skin fnt (DrawTextDef txtR wm T.empty AlignLeftTop txt)
{-    logPutLnWidget widget $ fromString $
                concat ["TandTItem.prepare srcR=", rectToBriefStr srcR, "\n inR=",rectToBriefStr inR,
                "\n txtLeftMarg=",show txtLeftMarg, "\n txtR=",rectToBriefStr txtR,
                "\n drawTextRect=",rectToBriefStr $ drawTextRect $ preparedTextDef prepTxt] -}
    return $ TandTItem (SDL.Rectangle srcP (V2 (srcW+(w-txtW)) (srcH+(h-txtH)))) txtrR texture prepTxt -- txtR

-- | Обновление 'TandTItem' при изменении размера.
update :: MonadIO m =>
                  Widget -> -- Skin ->
                  GuiRect -> -- ^ Весь элемент.
                  TandTItem ->
                  m TandTItem
update widget srcR TandTItem{..} =
    prepare widget srcR ttTexture (sizeOfRect ttPictRect) (drawTextWrap $ preparedTextDef ttPreparedText)
        (preparedTextFont ttPreparedText) (drawTextText $ preparedTextDef ttPreparedText)

-- | Отрисовка элемента
draw :: MonadIO m =>
            TandTItem ->
            GuiColor -> -- ^ Цвет текста.
            Int -> -- ^ Смещение по горизонтали области отображения текстуры. Умножается на ширину этой области.
            Bool -> -- ^ Это текущий, выбранный, элемент?
            Canvas m ()
draw TandTItem{..} color ix isCur = do
{-    setColor $ rgb 0 255 0
    drawRectWithDiagonals ttRect
    setColor $ rgb 0 0 255
    drawRectWithDiagonals $ drawTextRect $ preparedTextDef ttPreparedText  -- ttTextRect -}
    when isCur $ do
        setColor color
        drawDotBorder 4 $ shrinkRect' 3 ttRect
    let SDL.Rectangle pictP pictSz@(V2 pictW _) = ttPictRect
    drawTexturePartial ttTexture (SDL.Rectangle (P (V2 (fromIntegral ix * pictW) 0)) pictSz) pictP
    drawPreparedText ttPreparedText color Nothing

-- | Актуальные координаты элемента.
getRect :: TandTItem -> GuiRect
getRect = ttRect
{-# INLINE getRect #-}

-- | Актуальный размер области занимаемой элементом.
getSize :: TandTItem -> GuiSize
getSize = sizeOfRect . ttRect
{-# INLINE getSize #-}


-- | Находится ли указанная точка в границах отображения текстуры.
inPictRect :: TandTItem -> GuiPoint -> Bool
inPictRect tti = isInRect (ttPictRect tti)