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
--
-- А так же модальное окно с полем ввода текста.

module GUI.Window.MessageBox(
    -- * Типы описания модальных окон.
     MessageBox(..),MessageBoxType(..),MessageBoxButton(..),MessageBoxDef(..),MessageBoxOrdinary(..)
    ,TextInput(..)
    -- * Наиболее универсальное модальное окно.
    ,uniBox
    -- * Упрощённые варианты вызывающие @uniBox@ с информационным сообщением, предупреждением, ошибкой или
    -- требующее небольшого числа действия.
    ,messageBox,say
    -- * Модальное окно с полем ввода текста.
    ,textInput
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Char
import qualified Data.Text as T
import Data.Default
import Control.Monad.Extra (whenM,unlessM)
import qualified TextShow as TS
import qualified SDL
import SDL.Vect
import GUI
import GUI.Utils.TextWrap
import GUI.Widget.Label
import GUI.Widget.Container.Border
import GUI.Widget.Button
import GUI.Widget.Layout.LinearLayout
import GUI.Widget.EditBox

pattern MBoxFontKey :: T.Text; pattern MBoxFontKey = "label"
pattern DivisorHeight :: Coord; pattern DivisorHeight = 1
pattern MsgAreaWidth :: Coord; pattern MsgAreaWidth = 400
pattern BtnAreaHeight :: Coord; pattern BtnAreaHeight = 30

-- | Наиболее общий тип окна с сообщениями.
data MessageBox = MessageBox        MessageBoxOrdinary
                | MessageBoxInput   TextInput

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
--                    | MsgBoxLambda -- ^ Просто так :)
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
          msgBxCaption :: T.Text  -- ^ Заголовок окна. Если пуст, подбирается подходящее слово из текущего файла gui.txt.
        , msgBxPict :: T.Text  -- ^ Имя картинки из ресурсов. Если пустая строка, то:
                               --     * если поля ввода текста нет, картинка выбирается по 'MessageBoxType';
                               --     * если поле ввода есть, то картинка не вставляется.
        , msgBxDescr :: TS.Builder  -- ^ Описание, размещаемое в верхней области окна,
                                    -- возможно справа от большой картинки.
        , msgBx :: MessageBox -- ^ Основной выбор типа сообщения.
                                   }

instance Default MessageBoxDef where
    def = MessageBoxDef {
              msgBxCaption = T.empty
            , msgBxPict = T.empty
            , msgBxDescr = mempty
            , msgBx = MessageBox def
                        }

data MessageBoxOrdinary = MessageBoxOrdinary {
          msgBxType :: MessageBoxType
        , msgBxInitFocusedBtn :: Maybe MessageBoxButton -- ^ Кнопка, которая изначально в фокусе после создания окна.
          -- | Функция - - обработчик завершения, получает код нажатой клавишы или @MessageBoxNoButton@.
        , msgBxAccept :: forall m. MonadIO m => MessageBoxButton -> m ()
                                             }

instance Default MessageBoxOrdinary where
    def = MessageBoxOrdinary {
              msgBxType = MsgBoxOk
            , msgBxInitFocusedBtn = Nothing
            , msgBxAccept = \_ -> return ()
                             }

-- | Параметры окна с полем ввода текста
data TextInput = TextInput {
          textInputPrompt  :: T.Text -- ^ Текст подсказки левее поля ввода.
        , textInputWidth   :: Coord -- ^ Ширина поля редактирования. Высота определяется размером шрифта
                                   -- и внутренними полями фиксированного размера.
        , textInputText    :: T.Text -- ^ Исходный редактируемый текст.
        , textInputMaxChar :: Int -- ^ максимально допустимое число символов.
        , textInputCanEmpty :: Bool -- ^ Допустимо ли принимать пустую строку.
        , textInputCharFilter :: Char -> Bool -- ^ функция-фильтр допустимых для ввода символов.
                                             -- по умолчанию недопустимы пробельные символы кроме пробела.
        , textInputVerify :: forall m. MonadIO m => T.Text -> m Bool -- ^ Проверка на допустимость всего текста.
        -- | Функция - - обработчик завершения, получает введённый текст или Nothing при отмене.
        , textInputAccept :: forall m. MonadIO m => Maybe T.Text -> m ()
                           }

instance Default TextInput where
    def = TextInput {
              textInputPrompt = T.empty
            , textInputWidth = 300
            , textInputText = T.empty
            , textInputMaxChar = 100
            , textInputCanEmpty = False
            , textInputCharFilter = \c -> not (isSpace c) || c == ' '
            , textInputVerify = \_ -> return True
            , textInputAccept = \_ -> return ()
                    }

-- | Создаёт универсальное диалоговое окно опционально с большой картинкой из ресурса,
--   опционально с многострочным текстом комментария, опционально с полем ввода текста с подсказкой
--  и одним из наборов кнопок.
uniBox :: MonadIO m => Gui -> -- ^ Ссылка на GUIRecord
                       MessageBoxDef -> -- ^ Параметры сообщения.
                       m ()
uniBox gui MessageBoxDef{..} = do
    skin <- guiGetSkin gui
    let (MarginLTRB lMarg tMarg rMarg bMarg) = marginToLTRB $ formItemsMargin skin
        (mbt,maybeDefBtn,pictKey)=
          case msgBx of
            MessageBox MessageBoxOrdinary{..} ->
                let pict = if | not $ T.null msgBxPict -> msgBxPict
                              | mbt == MsgBoxOk -> "Messagebox_ok.png"
                              | mbt == MsgBoxInfo -> "Messagebox_info.png"
                              | (mbt == MsgBoxWarning) || (mbt == MsgBoxWarningYesNo) ||
                                (mbt == MsgBoxWarningOkCancel) ||
                                (mbt == MsgBoxWarningYesNoCancel) -> "Messagebox_warning.png"
--                              | mbt == MsgBoxLambda -> "Messagebox_lambda.png"
                              | (mbt == MsgBoxYesNo) || (mbt == MsgBoxOkCancel)
                                                          -> "Messagebox_question.png"
                              | otherwise -> "Messagebox_err.png"
                in (msgBxType,msgBxInitFocusedBtn,pict)
            MessageBoxInput _ -> (MsgBoxOkCancel,Just MessageBoxNoButton,msgBxPict)
        (btnLst,defBtn') = case mbt of
                            MsgBoxYesNo -> ([btYes,btNo],ButtonNo)
                            MsgBoxWarningYesNo -> ([btYes,btNo],ButtonNo)
                            MsgBoxOkCancel -> ([btOk,btCancel],ButtonCancel)
                            MsgBoxWarningOkCancel -> ([btOk,btCancel],ButtonCancel)
                            MsgBoxRetryCancel -> ([btRetry,btCancel],ButtonCancel)
                            MsgBoxRetrySkipCancel -> ([btRetry,btSkip,btCancel],ButtonCancel)
                            MsgBoxWarningYesNoCancel -> ([btYes,btNo,btCancel],ButtonNo)
                            MsgBoxErrorYesNoCancel -> ([btYes,btNo,btCancel],ButtonNo)
                            _ -> ([btOk],ButtonOk)
        defBtn = fromMaybe defBtn' maybeDefBtn
        descrT = TS.toText msgBxDescr
        cBtns = length btnLst
        onlyOneBtn = 1 == cBtns
        alignByBtns i = ((i+cBtns-1) `div` cBtns) *cBtns
    title <- if | not $ T.null msgBxCaption -> return msgBxCaption
                | mbt==MsgBoxOk -> getT gui "success"
                | mbt==MsgBoxInfo -> getT gui "information"
                | (mbt == MsgBoxWarning) || (mbt == MsgBoxWarningYesNo) ||
                  (mbt == MsgBoxWarningOkCancel) || (mbt == MsgBoxWarningYesNoCancel) -> getT gui "warning"
                | (mbt == MsgBoxYesNo) || (mbt == MsgBoxOkCancel) -> getT gui "confirm"
                | otherwise -> getT gui "error" -- MsgBoxError, MsgBoxRetryCancel, MsgBoxRetrySkipCancel
                                                -- MsgBoxErrorYesNoCancel
--                T.pack <$> liftIO getAppName
    win <- newModalWindow' gui title WindowCloseOnEsc SDL.defaultWindow{
                SDL.windowInitialSize = V2 1000 800 -- tmp.
            }
    vL <- win $+ vLayout def
    (w0,h0) <- if T.null pictKey && T.null descrT then return (0,0)
               else do  hL <- vL $+ hLayout def
                        (pictW,pictH) <-
                            if T.null pictKey then return (0,0)
                            else do pw <- hL $+ pictureWidget def{pictPicture = pictKey}
                                    (SDL.Rectangle _ (V2 w h)) <- getWidgetRectWithMargin $ baseWidget pw
                                    return (w,h)
                        (descrW,descrH) <-
                            if T.null descrT then return (0,0)
                            else do let dtdf = DrawTextDef  {
                                                  drawTextRect = SDL.Rectangle zero (V2 MsgAreaWidth 100)
                                                , drawTextWrap = TextWrap 600 Nothing
                                                , drawTextFontKey = MBoxFontKey
                                                , drawTextAlignment = AlignCenterTop
                                                , drawTextText = descrT }
                                    descrH <- (getHeightOfPreparedWrapText . preparedText) <$>
                                                prepareTextGui gui dtdf
                                    lw <- hL $+ label def {
                                              labelFormItemDef = def{
                                                formItemMargin=Just $ WidgetMarginLTRB 0 tMarg rMarg bMarg}
                                              , labelSize = V2 MsgAreaWidth descrH
                                              , labelFontKey = drawTextFontKey dtdf
                                              , labelAlignment=drawTextAlignment dtdf
                                              , labelWrapMode= drawTextWrap dtdf
                                              , labelText=drawTextText dtdf}
                                    (SDL.Rectangle _ (V2 w h)) <- getWidgetRectWithMargin $ baseWidget lw
                                    return (w,h)
                        return (pictW+descrW, max pictH descrH)
    (w1,h1,eBx) <- case msgBx of
                        MessageBox _ -> return (0,0,Nothing)
                        MessageBoxInput TextInput{..} -> do
                            hL <- vL $+ hLayout def
                            V2 lW lH <-
                                if T.null textInputPrompt then return zero
                                else do
                                    lbTxtSz <- runProxyWinCanvas win
                                        (getFont MBoxFontKey >>= (`getTextSize` textInputPrompt))
                                    lw <- hL $+ label def {
                                        labelFormItemDef = def{
                                            formItemMargin=Just $ WidgetMarginLTRB lMarg tMarg 0 bMarg}
                                      , labelSize = lbTxtSz
                                      , labelFontKey = MBoxFontKey
                                      , labelAlignment= AlignRightCenter
                                      , labelText=textInputPrompt}
                                    sizeOfRect <$> getWidgetRectWithMargin (baseWidget lw)
                            eb <- hL $+ editBox def{
                                      editBoxWidth = textInputWidth
                                    , editBoxText = textInputText
                                    , editBoxMaxChar = textInputMaxChar
                                    , editBoxCharFilter = textInputCharFilter
                                                   }
                            (SDL.Rectangle _ (V2 eW eH)) <- getWidgetRectWithMargin $ baseWidget eb
                            rf <- newMonadIORef textInputText
                            return (lW+eW, max lH eH, Just (eb,rf))

    let winW = alignByBtns $ max 200 $ max w0 w1
        winSz = V2 winW (h0 + h1 + DivisorHeight + BtnAreaHeight)
        btnW = winW `div` length btnLst

    mousePnt <- getDesktopMouseLocation
--    liftIO (putStr "uniBox : mousePnt=" >> print mousePnt)
    setWinNearer win mousePnt winSz

    void $ vL $+ border def{borderFormItemDef= def{formItemMargin=Just WidgetMarginNone}
                                                  , borderSize = V2 (-1) DivisorHeight}
    hL1 <- vL $+ hLayout def

    ~(bt0:_) <- forM btnLst $ \(btCode,pict,k) -> do
        txt <- getT gui k
        b <- hL1 $+ button def{ btnFormItemDef = def{formItemMargin=Just WidgetMarginNone}, btnUseBorder = False
                             , btnSize = V2 btnW BtnAreaHeight, btnPicture = ButtonLeftPicture pict
                             , btnText = txt}
        when ( (btCode == defBtn) || onlyOneBtn ) $
            setFocus b
        onClick b $ do
            setWinRetcode win $ WindowRetcode (fromEnum btCode)
            delWindow win
        return b

    case msgBx of
        MessageBox MessageBoxOrdinary{..} ->
            setWinOnClosed win (msgBxAccept . toEnum. unWindowRetcode)
        MessageBoxInput TextInput{..} -> do
            rfTerminated <- newMonadIORef False
            let (eb,rf) = fromJust eBx
                verifEmpty t = textInputCanEmpty || not (T.null t)
                verif t = (verifEmpty t && ) <$> textInputVerify t
                confirmIf t = unlessM (readMonadIORef rfTerminated) $ whenM (verif t)
                                (writeMonadIORef rf t >> writeMonadIORef rfTerminated True >>
                                 setWinRetcode win (WindowRetcode (fromEnum ButtonOk)) >>
                                 delWindow win)
            enableWidget bt0 =<< verif textInputText

            onEnd eb confirmIf

            onClick bt0
                (getText eb >>= confirmIf)

            setWinOnClosed win $ \rc ->
                (if ButtonOk == toEnum (unWindowRetcode rc) then
                    Just <$> readMonadIORef rf
                 else return Nothing) >>= textInputAccept

            setVerifier eb $ \t -> do
                b <- textInputVerify t
                enableWidget bt0 (b && verifEmpty t)
                return b

            setFocus eb

  where btOk = (ButtonOk,"button_ok.png","confirmB")
        btYes = (ButtonYes,"button_ok.png","yesB")
        btNo = (ButtonNo,"button_cancel.png","noB")
        btCancel = (ButtonCancel,"button_cancel.png","cancelB")
        btRetry = (ButtonRetry,"button_retry.png", "retryB")
        btSkip = (ButtonSkip,"button_skip.png", "skipB")


-- | Создаёт одно из распространённых модальных окон с информационным сообщением, предупреждением, ошибкой или
-- требующее небольшого числа действия.
messageBox :: MonadIO m => Gui -> -- ^ Ссылка на GUIRecord
                           MessageBoxType -> -- ^ Тип сообщения.
                           TS.Builder -> -- ^ Билдер из TextShow для упрощения формирования сложных сообщений.
                           -- | Функция - - обработчик завершения, получает код нажатой клавишы или @MessageBoxNoButton@.
                           (forall n. MonadIO n => MessageBoxButton -> n ()) ->
                           m ()
messageBox gui mbt descr answerFn =
    uniBox gui def{
          msgBxDescr = descr
        , msgBx = MessageBox def{
              msgBxType = mbt
            , msgBxAccept = answerFn
                                }
                  }

-- | Создаёт модальное окон с информационным сообщением, предупреждением или сообщением об ошибке.
-- Использует @messageBox@, но не требует указания функции обработки завершения.
-- Приеняется для типов сообщений с одной кнопкой в нижнем ряду.
say :: MonadIO m => Gui -> -- ^ Ссылка на GUIRecord
                    MessageBoxType -> -- ^ Тип сообщения.
                    TS.Builder -> -- ^ Билдер из TextShow для упрощения формирования сложных сообщений.
                    m ()
say gui mbt builder = messageBox gui mbt builder $ \_ -> return ()

-- | Создаёт модальное окно с полем ввода текста.
textInput :: MonadIO m => Gui -> -- ^ Ссылка на GUIRecord
                          T.Text -> -- ^ Заголовок окна.
                          TextInput -> -- ^ Дополнительные параметры.
                           m ()
textInput gui title ti =
    uniBox gui def{
          msgBxCaption = title
        , msgBx = MessageBoxInput ti
                  }