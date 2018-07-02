{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:      GUI.Widget.RadioButton
-- Copyright:   (c) 2017-2018 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- @radioButton@ виджет.

module GUI.Widget.RadioButton(
    -- * Типы для виджета @radioButton@
    RadioButtonItems(..) -- ^ Варианты задания диапазона и текста элементов.
                         -- Предпочтительно использовать смарт-конструкторы для создания экземпляров 'RadioButtonItems'.
    ,RadioButtonData -- ^ Тип созданного виджета @radioButton@.
    ,RadioButtonDef(..) -- ^ Параметры настройки виджета @radioButton@.
    -- * Смарт-конструкторы для определения 'RadioButtonItems'
    ,zeroBased,boundBased,zeroBasedS,boundBasedS,showableBased
    -- * Функция создания виджета @radioButton@.
    ,radioButton
    ) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.Bits
import Data.IORef
-- import Data.Monoid
-- import TextShow
import qualified SDL
import SDL.Vect
--import Control.Monad.Extra (whenJust)
import Data.Default
import GUI
import GUI.Widget.Handlers
import GUI.Utils.TextWrap
import qualified GUI.Utils.TandTItem as TTI

-- | Варианты задания диапазона и текста элементов.
-- Предпочтительно использовать смарт-конструкторы для создания экземпляров 'RadioButtonItems'.
data RadioButtonItems a = RadioButtonItems  { rbItmsMin :: a
                                            , rbItmsMax :: a
                                            , rbItmsTxts :: [T.Text]
                                            }

------- * Смарт-конструкторы для определения 'RadioButtonItems'

-- | Значения-индексы для выбора элемента будут 'Int' начинающиеся с 0.
zeroBased :: [T.Text] -> RadioButtonItems Int
zeroBased txts = RadioButtonItems 0 (length txts - 1) txts

-- | Индексация по типу реализующему Enum и Bounded.
boundBased :: forall a. (Enum a, Bounded a) => [T.Text] -> RadioButtonItems a
boundBased txts = RadioButtonItems minBound (toEnum $ min
                    (fromEnum (maxBound :: a) - fromEnum (minBound :: a)) $ length txts - 1)
                    txts

-- | Значения-индексы для выбора элемента будут 'Int' начинающиеся с 0. В отличии от @zeroBased@ тексты в 'String'.
zeroBasedS :: [String] -> RadioButtonItems Int
zeroBasedS  = zeroBased . map T.pack

-- | Индексация по типу реализующему Enum и Bounded.  В отличии от @boundBased@ тексты в 'String'.
boundBasedS :: (Enum a, Bounded a) => [String] -> RadioButtonItems a
boundBasedS = boundBased . map T.pack

-- | Отображение пунктов функцией @show@. Индексация по типу реализующему Enum и Bounded.
showableBased :: forall a. (Enum a, Bounded a, Show a) => RadioButtonItems a
showableBased = RadioButtonItems minBound maxBound [T.pack $ show a|a <-[minBound :: a .. maxBound]]

-- | Тип созданного виджета @radioButton@. Обычно используется как  @GuiWidget RadioButtonData@.
data RadioButtonData a = RadioButtonData    { radioButtonOnClick :: IORef (OneArgAction a)
                                            , radioButtonOnChanged :: IORef (OneArgAction a)
                                            , radioButtonChangeHandler :: forall m. MonadIO m => Widget -> a -> m ()
                                            , radioButtonRange :: (a,a)
                                            , radioButtonSelected :: IORef a
                                            }

-- | Установка функции-обработчика одинарного щелчка.
instance (Ord a, Enum a) => Clickable1 (GuiWidget (RadioButtonData a)) a where
    onClick1 w a = writeMonadIORef (radioButtonOnClick $ widgetData w) $ OneArgAction a

-- | Установка функции-обработчика на изменение состояния - выбранного элемента @radioButton@.
instance (Ord a, Enum a) => Changeable (GuiWidget (RadioButtonData a)) a where
    onChanged w a = writeMonadIORef (radioButtonOnChanged $ widgetData w) $ OneArgAction a

-- | Установка и извлечение состояния - выбранного элемента @radioButton@.
instance (Ord a, Enum a) => ValueProperty (GuiWidget (RadioButtonData a)) a where
    setValue w v = do
        old <- getValue w
        let (vmin,vmax) = radioButtonRange $ widgetData w
        when ( v /= old && v>=vmin && v<=vmax) $
            radioButtonChangeHandler (widgetData w) (baseWidget w) v

    getValue = readMonadIORef . radioButtonSelected . widgetData

-- | Параметры настройки виджета @radioButton@.
data RadioButtonDef = RadioButtonDef {
    radioButtonFormItemDef  :: FormItemWidgetDef -- ^ Общие настройки для всех виджетов для форм
                                                 -- в настоящий момент только margin's.
  , radioButtonSize         :: GuiSize -- ^ размер без полей.
  , radioButtonFlags        :: WidgetFlags -- ^ Флаги базового виджета.
  , radioButtonTextWrapMode :: TextWrapMode -- ^ Способ отображения надписей.
  , radioButtonMinItemHeight :: Coord -- ^ Минимальная высота пункта меню
                                            }

instance Default RadioButtonDef where
    def = RadioButtonDef { radioButtonFormItemDef = def
                        , radioButtonSize = zero
                        , radioButtonFlags = WidgetVisible .|. WidgetEnable .|. WidgetFocusable .|. WidgetTabbed
                        , radioButtonTextWrapMode = def
                        , radioButtonMinItemHeight = 10
                        }

-- | Функция создания виджета @radioButton@.
radioButton :: (Ord a, Enum a, MonadIO m) =>
                 RadioButtonDef ->      -- ^ Параметры виджета.
                 RadioButtonItems a ->  -- ^ Способы задания текста пунктов выбора и их нумерации.
                 a ->                   -- ^ Выбранный первоначально элемент.
                 Widget ->              -- ^ Будующий предок в дереве виджетов.
                 Skin ->                -- ^ Skin.
                 m (GuiWidget (RadioButtonData a))
radioButton RadioButtonDef{..} RadioButtonItems{..} firstSelected parent skin = do
    (texture,fnt,V2 txtrW txtrH) <- runProxyCanvas parent $ do
        t <- getTexture "radioButton.png"
        sz <- getTextureSize t
        f <- getFont "label"
        return (t,f,sz)
    let prepare :: MonadIO m => (GuiRect -> a -> m TTI.TandTItem) -> Coord -> [a] ->
                                    m (GuiSize,[TTI.TandTItem])
        prepare f width its = do
            (w,h,ts) <- foldM (\(limW,y,res) it -> do
                            t <- f (SDL.Rectangle (P(V2 0 y)) (V2 width radioButtonMinItemHeight)) it
                            let V2 w h = TTI.getSize t
                            return (max limW w,y+h,t:res)
                                ) (0,0,[]) its
            return (V2 w h, reverse ts)
        pictSz = V2 (txtrW `div` 4) txtrH
    initT@(initSz,_) <- prepare (\r -> TTI.prepare parent r texture pictSz radioButtonTextWrapMode fnt)
                                              (xV2 radioButtonSize) rbItmsTxts
    ttiRf <- newMonadIORef initT
    stateRf <- newMonadIORef firstSelected
    onCLickRf <- newMonadIORef $ OneArgAction $ \_ -> return ()
    onChangeRf <- newMonadIORef $ OneArgAction $ \_ -> return ()
    let changeHandler widget newV = do
            writeMonadIORef stateRf newV
            markWidgetForRedraw widget
            readMonadIORef onChangeRf >>= ( $ newV) . oneArgAction
        isHotSpot widget pnt = do
            let go _ [] = return False
                go n (t:ts) | TTI.inPictRect t pnt = do
                                    old <- readMonadIORef stateRf
                                    when (old /= n) $ do
                                        changeHandler widget n
                                        readMonadIORef onCLickRf >>= ( $ n) . oneArgAction
                                    return (old /= n)
                            | otherwise = go (succ n) ts
            go rbItmsMin . snd =<< readMonadIORef ttiRf
        doMove widget f = do
            readMonadIORef stateRf >>= changeHandler widget . f
    ClickableHelper{ clickableFs = fns} <- clickableHelper initSz (\_ -> return ()) isHotSpot
    mkFormWidget radioButtonFormItemDef radioButtonFlags skin id
            (RadioButtonData onCLickRf onChangeRf changeHandler (rbItmsMin,rbItmsMax) stateRf) parent fns{
         onDraw= \widget -> do
            fl <- getWidgetFlags widget
            let ena = (fl .&. WidgetEnable) == WidgetEnable
                focused = ena && ((fl .&. WidgetFocused) == WidgetFocused)
                decore = formDecore skin
                foregroundColor = decoreFgColor decore
                ix = if ena then 0 else 1
            s  <- readMonadIORef stateRf
            r <- getVisibleRect widget
{-            logPutLnWidget widget $ fromString $
                concat ["radioButton.onDraw getVisibleRect=", rectToBriefStr r] -}
            setColor $ decoreBkColor decore
            fillRect r
            let

--            setColor $ rgb 0 0 0
--            drawRectWithDiagonals
--            fillRect $ SDL.Rectangle (P(V2 169 31)) (V2 10 10)
            (_,tti's) <- readMonadIORef ttiRf
            forM_ (zip tti's [rbItmsMin..])  $ \(tti,n) ->
                TTI.draw tti foregroundColor (ix + if s==n then 2 else 0) (focused && s==n)
        ,onResizing= \widget newRect@(SDL.Rectangle newP newSz) -> do
            marg <- getWidgetMargin widget
            (oldSz,oldTTI) <- readMonadIORef ttiRf
            let margSz = marginSize marg
                inSz = newSz - margSz
{-            let rectToBriefT = fromString . rectToBriefStr
--            old <- getWidgetRectWithMargin widget
            logPutLnWidget widget $ "radioButton.onResizing  T=" <> (showb $ T.take 1 radioButtonText)
                <> " newRect=" <> rectToBriefT newRect  <>
                " inSz=" <> rectToBriefT inSz <> " oldR=" <> rectToBriefT oldR -}
            r <- if inSz /= oldSz
                 then do
                    newT@(sz,_) <- prepare (TTI.update widget) (xV2 inSz) oldTTI
{-                    logPutLnWidget widget $ "radioButton.onResizing  T=" <> (showb $ T.take 1 radioButtonText)
                        <> " newTTIRect=" <> rectToBriefT newTTIRect -}
                    if sz /= oldSz
                    then do
                        writeMonadIORef ttiRf newT
                        let rr = SDL.Rectangle newP $ sz + margSz
{-                        logPutLnWidget widget $ "radioButton.onResizing  T=" <> (showb $ T.take 1 radioButtonText)
                            <> " rr=" <> rectToBriefT rr -}
                        return rr
                    else return newRect
                 else return newRect
            onResizing fns widget r
       ,onKeyboard = \widget motion repeated keycode km ->
            if motion==SDL.Pressed && (ShiftCtrlAlt False False False) == getShftCtrlAlt km
            then case keycode of
                SDL.KeycodeUp -> doMove widget $ \s -> if s == rbItmsMin then rbItmsMax else pred s
                SDL.KeycodeDown -> doMove widget $ \s -> if s == rbItmsMax then rbItmsMin else succ s
                _ -> onKeyboard fns widget motion repeated keycode km
            else onKeyboard fns widget motion repeated keycode km
                                                   }


