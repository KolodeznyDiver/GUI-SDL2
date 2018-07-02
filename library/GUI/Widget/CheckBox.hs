{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:      GUI.Widget.CheckBox
-- Copyright:   (c) 2017-2018 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- @checkBox@ виджет.

module GUI.Widget.CheckBox(
    CheckBoxData,CheckBoxDef(..)
    ,checkBox
    ) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.Bits
import Data.IORef
{-
import Data.Monoid
import TextShow
-}
import qualified SDL
import SDL.Vect
--import Control.Monad.Extra (whenJust)
import Data.Default
import GUI
import GUI.Widget.Handlers
import GUI.Utils.TextWrap
import qualified GUI.Utils.TandTItem as TTI

-- | Тип созданного виджета @checkBox@. Обычно используется как  @GuiWidget CheckBoxData@.
data CheckBoxData = CheckBoxData { checkBoxOnClick :: IORef (OneArgAction Bool)
                                 , checkBoxState :: IORef Bool
                                 , checkBoxOnChanged :: IORef (OneArgAction Bool)
                                 , checkBoxChangeHandler :: forall m. MonadIO m => Widget -> Bool -> m ()
                                 }

-- | Установка функции-обработчика одинарного щелчка.
instance Clickable1 (GuiWidget CheckBoxData) Bool where
    onClick1 w a = writeMonadIORef (checkBoxOnClick $ widgetData w) $ OneArgAction a

-- | Установка функции-обработчика на изменение состояния отмечено (\'checked\').
instance Changeable (GuiWidget CheckBoxData) Bool where
    onChanged w a = writeMonadIORef (checkBoxOnChanged $ widgetData w) $ OneArgAction a

-- | Установка и извлечение состояния отмечено (\'checked\').
instance ValueProperty (GuiWidget CheckBoxData) Bool where
    setValue w v = do
        old <- getValue w
        when ( v /= old) $
            checkBoxChangeHandler (widgetData w) (baseWidget w) v

    getValue = readMonadIORef . checkBoxState . widgetData

-- | Параметры настройки виджета @checkBox@.
data CheckBoxDef = CheckBoxDef {
    checkBoxFormItemDef  :: FormItemWidgetDef -- ^ Общие настройки для всех виджетов для форм
                                              -- в настоящий момент только margin's.
  , checkBoxSize      :: GuiSize -- ^ размер без полей.
  , checkBoxFlags     :: WidgetFlags -- ^ Флаги базового виджета.
  , checkBoxChecked :: Bool -- ^ Устанавливать ли \'галочку\' в начале.
  , checkBoxTextWrapMode :: TextWrapMode -- ^ Способ отображения надписи.
  , checkBoxText  :: T.Text -- ^ Собственно надпись.
                                            }

instance Default CheckBoxDef where
    def = CheckBoxDef   { checkBoxFormItemDef = def{formItemMargin=Just $ WidgetMarginXY 10 0}
                        , checkBoxSize = zero
                        , checkBoxFlags = WidgetVisible .|. WidgetEnable .|. WidgetFocusable .|. WidgetTabbed
                        , checkBoxChecked = False
                        , checkBoxTextWrapMode = def
                        , checkBoxText = T.empty
                        }

-- | Создание виджета @checkBox@.
checkBox :: MonadIO m =>
                 CheckBoxDef ->  -- ^ Параметры виджета.
                 Widget ->  -- ^ Будующий предок в дереве виджетов.
                 Skin -> -- ^ Skin.
                 m (GuiWidget CheckBoxData)
checkBox CheckBoxDef{..} parent skin = do
    (texture,fnt,V2 txtrW txtrH) <- runProxyCanvas parent $ do
        t <- getTexture "checkBox.png"
        sz <- getTextureSize t
        f <- getFont "label"
        return (t,f,sz)
    tti <- TTI.prepare parent
            (SDL.Rectangle zero checkBoxSize) texture
            (V2 (txtrW `div` 4) txtrH) checkBoxTextWrapMode fnt checkBoxText
    ttiRf <- newMonadIORef tti
    stateRf <- newMonadIORef checkBoxChecked
    onCLickRf <- newMonadIORef $ OneArgAction $ \_ -> return ()
    onChangeRf <- newMonadIORef $ OneArgAction $ \_ -> return ()
    let changeHandler widget newV = do
            writeMonadIORef stateRf newV
            markWidgetForRedraw widget
            readMonadIORef onChangeRf >>= ( $ newV) . oneArgAction
        clickHandler widget = do
            newS <- not <$> readMonadIORef stateRf
            changeHandler widget newS
            readMonadIORef onCLickRf >>= ( $ newS) . oneArgAction
        isHotSpot _ = return . TTI.inPictRect tti
    ClickableHelper{ clickableFs = fns}
        <- clickableHelper (TTI.getSize tti) clickHandler isHotSpot
    mkFormWidget checkBoxFormItemDef checkBoxFlags skin id
            (CheckBoxData onCLickRf stateRf onChangeRf changeHandler) parent fns{
         onDraw= \widget -> do
            fl <- getWidgetFlags widget
            let ena = (fl .&. WidgetEnable) == WidgetEnable
                focused = ena && ((fl .&. WidgetFocused) == WidgetFocused)
            s  <- readMonadIORef stateRf
            let ix = (if ena then 0 else 1) + (if s then 2 else 0)
            r <- getVisibleRect widget
{-            logPutLnWidget widget $ fromString $
                concat ["checkBox.onDraw getVisibleRect=", rectToBriefStr r] -}
            setColor $ decoreBkColor $ formDecore skin
            fillRect r
--            setColor $ rgb 0 0 0
--            drawRectWithDiagonals
--            fillRect $ SDL.Rectangle (P(V2 169 31)) (V2 10 10)
            tti' <- readMonadIORef ttiRf
            TTI.draw tti' (decoreFgColor (formDecore skin)) ix focused
        ,onResizing= \widget newRect@(SDL.Rectangle newP newSz) -> do
            marg <- getWidgetMargin widget
            oldTTI <- readMonadIORef ttiRf
            let oldTTIRect = TTI.getRect oldTTI
                margSz = marginSize marg
                inR = SDL.Rectangle zero $ newSz - margSz
{-            let rectToBriefT = fromString . rectToBriefStr
--            old <- getWidgetRectWithMargin widget
            logPutLnWidget widget $ "checkBox.onResizing  T=" <> (showb $ T.take 1 checkBoxText)
                <> " newRect=" <> rectToBriefT newRect  <>
                " inR=" <> rectToBriefT inR <> " oldTTIRect=" <> rectToBriefT oldTTIRect -}
            r <- if inR /= oldTTIRect
                 then do
                    newTTI <- TTI.update widget inR oldTTI
                    let newTTIRect@(SDL.Rectangle newTTIP newTTISz) = TTI.getRect newTTI
{-                    logPutLnWidget widget $ "checkBox.onResizing  T=" <> (showb $ T.take 1 checkBoxText)
                        <> " newTTIRect=" <> rectToBriefT newTTIRect -}
                    if newTTIRect /= oldTTIRect
                    then do
                        writeMonadIORef ttiRf newTTI
                        let rr = SDL.Rectangle (newP + newTTIP) $ newTTISz + margSz
{-                        logPutLnWidget widget $ "checkBox.onResizing  T=" <> (showb $ T.take 1 checkBoxText)
                            <> " rr=" <> rectToBriefT rr -}
                        return rr
                    else return newRect
                 else return newRect
            onResizing fns widget r
                                                   }
