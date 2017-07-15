{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:      GUI.Window.MessageBox
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <kolodeznydiver@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Часто используемые модальные окна с сообщением и несколькими общепринятыми для таких сообщений кнопками :
-- "Подтвердить","Отменить", "Повторить".
--
-- В отличии от @GUI.BaseLayer.Depend0.Auxiliaries.showErrMsgBoxB@ имеют возможность настройки и поддерживают 'Skin',
-- но треботают что бы GUI находился в рабочем состоянии, к примеру, должны быь загружены шрифты.

module GUI.Window.MessageBox(
    MessageBoxType(..),MessageBoxButton(..),MessageBoxDef(..),messageBox,say
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Maybes (whenIsJust)
import qualified Data.Text as T
import Data.Bits
import Data.IORef
import Data.Maybe
import Data.Default
import qualified TextShow as TS
import qualified SDL
import SDL.Vect
import GUI
import GUI.BaseLayer.Primitives as P
import GUI.Utils.TextWrap
import GUI.Widget.Handlers
import GUI.Widget.Label
import GUI.Widget.Container.Border
import GUI.Widget.Button
import GUI.Widget.Layout.LinearLayout

pattern PaddingX :: Coord; pattern PaddingX = 15
pattern PaddingY :: Coord; pattern PaddingY = 5
pattern BigPictSz :: Coord; pattern BigPictSz = 48
pattern DivisorHeight :: Coord; pattern DivisorHeight = 1
pattern MsgAreaWidth :: Coord; pattern MsgAreaWidth = 400
pattern BtnAreaHeight :: Coord; pattern BtnAreaHeight = 30

-- | Тип сообщения. Определяет и набор кнопок сообщения и картинку слева от основного текста сообщения.
data MessageBoxType = MsgBoxOk
                    | MsgBoxInfo
                    | MsgBoxWarning
                    | MsgBoxError
                    | MsgBoxYesNo
                    | MsgBoxWarningYesNo
                    | MsgBoxOkCancel
                    | MsgBoxWarningOkCancel
                    | MsgBoxRetryCancel
                    | MsgBoxRetrySkipCancel
                    | MsgBoxWarningYesNoCancel
                    | MsgBoxErrorYesNoCancel
                    | MsgBoxLambda -- ^ Просто так :)
                    deriving Eq

-- | Код нажатой клавишы, который передаётся в фукнцию - обработчик завершения.
data MessageBoxButton = MessageBoxNoButton -- ^ Окно закрыто иным спсобом.
                      | ButtonOk
                      | ButtonYes
                      | ButtonNo
                      | ButtonCancel
                      | ButtonRetry
                      | ButtonSkip
                      deriving (Eq, Enum)

-- | Дополнительные параметры окна с сообщениями.
data MessageBoxDef = MessageBoxDef {
        messageBoxInitFocusedBtn :: MessageBoxButton -- ^ Кнопка, которая изначально в фокусе после создания окна.
                                   }

instance Default MessageBoxDef where
    def = MessageBoxDef MessageBoxNoButton

-- | Создаёт одно из распространённых модальных окон с информационным сообщением, предупреждением, ошибкой или
-- требующее небольшого числа действия.
messageBox :: MonadIO m => Gui -> -- ^ Ссылка на GUIRecord
                           MessageBoxType -> -- ^ Тип сообщения.
                           TS.Builder -> -- ^ Билдер из TextShow для упрощения формирования сложных сообщений.
                           MessageBoxDef -> -- ^ Дополнительные параметры сообщения.
                           -- | Функция - - обработчик завершения, получает код нажатой клавишы или @MessageBoxNoButton@.
                           (forall n. MonadIO n => MessageBoxButton -> n ()) ->
                           m ()
messageBox gui mbt builder MessageBoxDef{..} answerFn = do
    let dtdf = DrawTextDef  {
                drawTextRect = SDL.Rectangle zero (V2 MsgAreaWidth 100)
                , drawTextWrap = TextWrap 600 Nothing
                , drawTextFontKey = "label"
                , drawTextAlignment = AlignCenterTop
                , drawTextText = TS.toText builder }
    title <- T.pack <$> liftIO getAppName
    p <- prepareTextGui gui dtdf
    let btnLst = case mbt of
                    MsgBoxYesNo -> [btYes,btNo]
                    MsgBoxWarningYesNo -> [btYes,btNo]
                    MsgBoxOkCancel -> [btOk,btCancel]
                    MsgBoxWarningOkCancel -> [btOk,btCancel]
                    MsgBoxRetryCancel -> [btRetry,btCancel]
                    MsgBoxRetrySkipCancel -> [btRetry,btSkip,btCancel]
                    MsgBoxWarningYesNoCancel -> [btYes,btNo,btCancel]
                    MsgBoxErrorYesNoCancel -> [btYes,btNo,btCancel]
                    _ -> [btOk]
        btnW = winW `div` length btnLst
        cBtns = length btnLst
        onlyOneBtn = 1 == cBtns
        alignByBtns i = ((i+cBtns-1) `div` cBtns) *cBtns
        winW = alignByBtns $ PaddingX*3+BigPictSz+MsgAreaWidth
        txtH = getHeightOfPreparedWrapText $ preparedText p
    win <- newModalWindow gui title SDL.defaultWindow{
            SDL.windowInitialSize =
                P.toSDLV2 $ V2 winW (PaddingY*2 + DivisorHeight + BtnAreaHeight + max BigPictSz txtH)
            }
    setWinOnClosed win (answerFn . toEnum. unWindowRetcode)
    vL <- win $+ vLayout def
    hL0 <- vL $+ hLayout def
    void $ hL0 $+ pictureWidget def{pictFormItemDef = def{formItemMargin= Just $ WidgetMarginXY PaddingX PaddingY}
                                   , pictPicture = if | mbt == MsgBoxOk -> "Messagebox_ok.png"
                                                      | mbt == MsgBoxInfo -> "Messagebox_info.png"
                                                      | (mbt == MsgBoxWarning) || (mbt == MsgBoxWarningYesNo) ||
                                                        (mbt == MsgBoxWarningOkCancel) ||
                                                        (mbt == MsgBoxWarningYesNoCancel)
                                                          -> "Messagebox_warning.png"
                                                      | mbt == MsgBoxLambda -> "Messagebox_lambda.png"
                                                      | (mbt == MsgBoxYesNo) || (mbt == MsgBoxOkCancel)
                                                          -> "Messagebox_question.png"
                                                      | otherwise -> "Messagebox_err.png"
                                   }
    void $ hL0 $+ label def { labelFormItemDef = def{formItemMargin=Just $ WidgetMarginLTRB 0 PaddingY PaddingX PaddingY}
                            , labelSize = V2 MsgAreaWidth txtH, labelFontKey = drawTextFontKey dtdf
                            , labelAlignment=drawTextAlignment dtdf, labelWrapMode= drawTextWrap dtdf
                            , labelText=drawTextText dtdf}
    void $ vL $+ border def{borderFormItemDef= def{formItemMargin=Just WidgetMarginNone}
                                                  , borderSize = V2 (-1) DivisorHeight}
    hL1 <- vL $+ hLayout def

    forM_ btnLst $ \(btCode,pict,k) -> do
        txt <- getT gui k
        b <- hL1 $+ button def{ btnFormItemDef = def{formItemMargin=Just WidgetMarginNone}, btnUseBorder = False
                             , btnSize = V2 btnW BtnAreaHeight, btnPicture = ButtonLeftPicture pict
                             , btnText = txt}
        when ( (btCode == messageBoxInitFocusedBtn) || onlyOneBtn ) $
            setWidgetFocus $ getWidget b
        onClick b $ do
            setWinRetcode win $ WindowRetcode (fromEnum btCode)
            delWindow win
  where btOk = (ButtonOk,"button_ok.png","confirmB")
        btYes = (ButtonYes,"button_ok.png","yesB")
        btNo = (ButtonNo,"button_cancel.png","noB")
        btCancel = (ButtonCancel,"button_cancel.png","cancelB")
        btRetry = (ButtonRetry,"button_retry.png", "retryB")
        btSkip = (ButtonSkip,"button_skip.png", "skipB")

-- | Создаёт модальное окон с информационным сообщением, предупреждением или сообщением об ошибке.
-- Использует @messageBox@, но не требует указания дополнительных параметров и функции обработки завершения.
-- Приеняется для типов сообщений с одной кнопкой в нижнем ряду.
say :: MonadIO m => Gui -> -- ^ Ссылка на GUIRecord
                    MessageBoxType -> -- ^ Тип сообщения.
                    TS.Builder -> -- ^ Билдер из TextShow для упрощения формирования сложных сообщений.
                    m ()
say gui mbt builder = messageBox gui mbt builder def $ \_ -> return ()
