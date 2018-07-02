{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:      GUI.Widget.Label
-- Copyright:   (c) 2017-2018 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Надпись на форме. После создания может быть изменён текст и цвет.
-- Текст может быть многострочным с переносом, см. "GUI.Utils.TextWrap".

module GUI.Widget.Label(
    -- Реэкспорт из "GUI.Util.TextWrap"
    TextWrapMode(..),DrawTextDef(..),PreparedText(..)
    -- * Типы для label.
    ,LabelDef(..),LabelData
    -- * Функция создания текстовой надписи.
    ,label
    ) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.Bits
import Data.IORef
import Data.Maybe
import Data.Default
import qualified SDL
import SDL.Vect
import GUI
import GUI.Utils.TextWrap
import GUI.Widget.Handlers

-- | Параметры создаваемой надписи.
data LabelDef = LabelDef {
          labelFormItemDef  :: FormItemWidgetDef -- ^ Общие настройки для всех виджетов для форм,
                                                 -- в настоящий момент только margin's.
        , labelSize      :: GuiSize -- ^ Минимальные размеры виджета.
        , labelFlags     :: WidgetFlags -- ^ Флаги базового виджета.
        , labelAlignment :: Alignment -- ^ Выравнивание текста в пределах заданной области.
        , labelFontKey :: T.Text -- ^ Шрифт текста, по умолчанию \"label\"
                                 -- (весьма желательно добавлять элемент с таким ключом в таблицу шрифтов).
        , labelColor :: Maybe GuiColor -- ^ Начальный цвет текста или @decoreFgColor (formDecore skin)@.
        , labelBkColor :: Maybe GuiColor -- ^ Цвет фона или @decoreBkColor (formDecore skin)@.
        , labelWrapMode :: TextWrapMode -- ^ Режим прокрутки текста. По умолчанию без переносов строк и без
                                        -- увеличения размера виджета.
        , labelText  :: T.Text -- ^ Отображаемый текст.
                            }

instance Default LabelDef where
    def = LabelDef { labelFormItemDef = def
                   , labelSize = zero
                   , labelFlags = WidgetVisible .|. WidgetEnable
                   , labelAlignment = AlignLeftCenter
                   , labelFontKey = "label"
                   , labelColor = Nothing
                   , labelBkColor = Nothing
                   , labelWrapMode = def
                   , labelText = T.empty
                   }

-- | Тип созданного виджета. Обычно используется как  @GuiWidget LabelData@.
data LabelData = LabelData { labelPreparedText :: IORef PreparedText
                           , labelTextColor :: IORef GuiColor
                           }

-- | label поддерживает возможность получить и изменить свою текстовую строку.
instance TextProperty (GuiWidget LabelData) where
    setText (GuiWidget widget LabelData{..}) txt = do
        PreparedText{..} <- readMonadIORef labelPreparedText
        when (txt /= drawTextText preparedTextDef) $ do
            p <- prepareText widget preparedTextDef{drawTextText=txt}
            writeMonadIORef labelPreparedText p
            markWidgetForRedraw widget
    getText (GuiWidget _ LabelData{..}) = (drawTextText . preparedTextDef) <$> readMonadIORef labelPreparedText


-- | label поддерживает возможность получить и изменить цвет своего текста.
instance TextColorProperty (GuiWidget LabelData) where
    setTextColor (GuiWidget widget LabelData{..}) color = do
        oc <- readMonadIORef labelTextColor
        when (color /= oc) $ do
            writeMonadIORef labelTextColor color
            markWidgetForRedraw widget
    getTextColor (GuiWidget _ LabelData{..}) = readMonadIORef labelTextColor

-- | Функция создания текстовой надписи.
label :: MonadIO m => LabelDef -> -- ^ Параметры виджета.
                      Widget -> -- ^ Будующий предок в дереве виджетов.
                      Skin -> -- ^ Skin.
                      m (GuiWidget LabelData)
label LabelDef{..} parent skin = do
    p <- prepareText parent DrawTextDef  { drawTextRect = SDL.Rectangle zero $
                                                    sizeReplaceIfNoPositive (V2 100 100) labelSize
                                                , drawTextWrap = labelWrapMode
                                                , drawTextFontKey = labelFontKey
                                                , drawTextAlignment = labelAlignment
                                                , drawTextText = labelText
                                                                   }
    let insideSz = unP $ getRectRB $ drawTextRect $ preparedTextDef p
--    dbgCnt <- newMonadIORef (0::Int)
    prepRf <- newMonadIORef p
    colorRf <- newMonadIORef $ fromMaybe (decoreFgColor (formDecore skin)) labelColor
    let bkgrndColor = fromMaybe (decoreBkColor (formDecore skin)) labelBkColor
        bkTxtColor = textWrapModeToMbBkColor labelWrapMode skin bkgrndColor
        fns = noChildrenFns $ sizeRestoreNegative labelSize insideSz
    mkFormWidget labelFormItemDef labelFlags skin id (LabelData prepRf colorRf) parent fns{
        onDraw= \widget -> do
            ena <- allWidgetFlags widget WidgetEnable
            c <- if ena then readMonadIORef colorRf else return $ formDisabledFgColor skin
            r <- getVisibleRect widget
--            liftIO $ putStrLn $ concat ["label.onDraw getVisibleRect=", rectToBriefStr r]
            setColor bkgrndColor
            fillRect r
            prep <- readMonadIORef prepRf
            drawPreparedText prep c bkTxtColor
--            setColor $ rgb 255 0 0
--            drawRect $ shrinkRect' 1 r
        ,onResizing= \widget newRect -> do
{-            old <- getWidgetRectWithMargin widget
            liftIO $ putStrLn $ concat ["label.onResizing  newRect=", rectToBriefStr newRect,
                "  old=", rectToBriefStr old]
            modifyMonadIORef' dbgCnt succ
            cnt <- readMonadIORef dbgCnt
            when (cnt>10) (getWidgetWindow widget >>= delWindow >> (liftIO exitFailure)) -}
            onResizing fns widget newRect
            PreparedText{..} <- readMonadIORef prepRf
--            liftIO $ putStrLn $ concat ["label.onResizing  "]
            when (sizeOfRect newRect /= sizeOfRect (drawTextRect preparedTextDef)) $ do
                r <- getWidgetCanvasRect widget
                prepareText widget preparedTextDef{drawTextRect=r} >>= writeMonadIORef prepRf
                                                }
