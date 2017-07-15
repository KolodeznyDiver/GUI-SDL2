{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
-- |
-- Module:      GUI.Widget.TextEdit
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <kolodeznydiver@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Поле для редактирования текста.
-- Использует шрифт \"edit\" из таблицы шрифтов менеджера ресурсов.

module GUI.Widget.TextEdit(
    -- * Типы поля для редактирования текста.
    TextEditDef(..),TextEditData
    -- * Функция создания виджета для редактирования текста.
    ,textEdit
    -- * Функции установки обработчиков событий специфичных для виджета.
    ,onTextEditAtEnd,setTextEditVerifier
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Bits
import Data.Char
import Data.IORef
import Data.Maybe
import MonadUtils (whenM)
import qualified SDL
import SDL.Vect
import qualified SDL.TTF as TTF
import Maybes (whenIsJust)
import Data.Default
import GUI
import qualified GUI.BaseLayer.Primitives as P
import GUI.Widget.Handlers

pattern PaddingX :: Coord
pattern PaddingX = 5
pattern PaddingY :: Coord
pattern PaddingY = 3

-- | Начальные настройки виджета для редактирования текста.
data TextEditDef = TextEditDef {
          textEditFormItemDef  :: FormItemWidgetDef -- ^ Общие настройки для всех виджетов для форм,
                                                    -- в настоящий момент только margin's.
        , textEditWidth   :: Coord -- ^ Ширина поля редактирования. Высота определяется размером шрифта
                                   -- и внутренними полями фиксированного размера.
        , textEditFlags   :: WidgetFlags -- ^ Флаги базового виджета.
        , textEditText    :: T.Text -- ^ Исходный текст.
        , textEditMaxChar :: Int -- ^ максимально допустимое число символов.
        , textEditCharFilter :: Char -> Bool -- ^ функция-фильтр допустимых для ввода символов.
                                             -- по умолчанию недопустимы пробельные символы кроме пробела.
                               }

instance Default TextEditDef where
    def = TextEditDef { textEditFormItemDef = def
                      , textEditWidth = 100
                      , textEditFlags = WidgetVisible .|. WidgetEnable .|. WidgetFocusable .|. WidgetTabbed
                      , textEditText = T.empty
                      , textEditMaxChar = 100
                      , textEditCharFilter = \c -> not (isSpace c) || c == ' '
                      }

-- | Не экспортируемый тип записи. Хранится по ссылке в 'TextEditData'.
data TextEditStruct = TextEditStruct    { txtEdText :: T.Text
                                        , txtEdOnChanged :: forall m. MonadIO m => T.Text -> m ()
                                        , txtEdAtEnd :: forall m. MonadIO m => T.Text -> m ()
                                        , txtEdVerif :: forall m. MonadIO m => T.Text -> m Bool
--                                        , txtEdCharVerif :: forall m. MonadIO m => Char -> m Bool
                                        , txtEdSetText :: forall m. MonadIO m => T.Text -> m Bool
                                        }

-- | Тип созданного виджета. Обычно используется как  @GuiWidget TextEditData@.
newtype TextEditData = TextEditData { getTxtEd :: IORef TextEditStruct }

-- | Установка функции-обработчика на изменение текста.
instance Changeable (GuiWidget TextEditData) T.Text where
    onChanged w a = modifyMonadIORef' (getTxtEd $ getWidgetData w) (\d -> d{txtEdOnChanged= a})

-- | Установка функции-обработчика вызываемого когда поле редактирования теряет фокус или нажат Enter.
onTextEditAtEnd :: forall m. MonadIO m => GuiWidget TextEditData ->
                            (forall n. MonadIO n => T.Text -> n ()) -> m ()
onTextEditAtEnd w a = modifyMonadIORef' (getTxtEd $ getWidgetData w) (\d -> d{txtEdAtEnd= a})
{-# INLINE onTextEditAtEnd #-}

-- | Установка предиката-верификатора. Он вызывается при любой ПОПЫТКЕ изменения текста.
-- если предикат вернёт False, текст не будет изменён.
setTextEditVerifier :: forall m. MonadIO m => GuiWidget TextEditData ->
                            (forall n. MonadIO n => T.Text -> n Bool) -> m ()
setTextEditVerifier w a = modifyMonadIORef' (getTxtEd $ getWidgetData w) (\d -> d{txtEdVerif= a})
{-# INLINE setTextEditVerifier #-}
{-
setTextEditCharVerifier :: forall m. MonadIO m => GuiWidget TextEditData ->
                            (forall n. MonadIO n => Char -> n Bool) -> m ()
setTextEditCharVerifier w a = modifyMonadIORef' (getTxtEd $ getWidgetData w) (\d -> d{txtEdCharVerif= a})
{-# INLINE setTextEditCharVerifier #-}
-}

-- | Установка и извлечение редактируемого текста.
instance TextProperty (GuiWidget TextEditData) where
    setText (GuiWidget widget TextEditData{..}) txt = do
        TextEditStruct{..} <- readMonadIORef getTxtEd
        when (txt /= txtEdText) $
            whenM (txtEdSetText txt) $
                markWidgetForRedraw widget
    getText (GuiWidget _ TextEditData{..}) = txtEdText <$> readMonadIORef getTxtEd


-- no export
widthSum :: Int -> Int -> VU.Vector Coord -> Coord
widthSum from to | from < to = VU.sum . VU.slice from (to - from)
                 | otherwise = const 0
{-# INLINE widthSum #-}

-- internal for textEdit
data TextEditState = TextEditState      { txtEdPos :: Int -- Caret pos. or start selection
                                        , txtEdSel :: Int -- stop selection or -1
                                        , txtEdOff :: Int -- view area offset in Chars
                                        , txtEdWidths :: VU.Vector Coord
                                        }

-- | Функция создания виджета для редактирования текста.
textEdit :: MonadIO m => TextEditDef -> -- ^ Параметры виджета.
                         Widget -> -- ^ Будующий предок в дереве виджетов.
                         Skin -> -- ^ Skin.
                         m (GuiWidget TextEditData)
textEdit TextEditDef{..} parent skin = do
    fnt <- runProxyCanvas parent $ getFont "edit"
    fntHeight <- TTF.getFontHeight fnt

    let allSz = V2 textEditWidth (fntHeight + 2*PaddingY)
        fns = noChildrenFns allSz
        txtAreaW = textEditWidth - 2*PaddingY

        calcMinOff :: VU.Vector Coord -> Coord -> Int -> Int
        calcMinOff v = go
            where go _ 0 = 0
                  go r i = let  i' = i - 1
                                r' = r - v VU.! i'
                           in if r' <= 0 then i else go r' i'

        adjustOffset :: TextEditState -> TextEditState
        adjustOffset s@TextEditState{..}
            | (txtEdPos < txtEdOff) || ((txtEdPos == txtEdOff) && (txtEdOff > 0)) =
                            s{txtEdOff= calcMinOff txtEdWidths (txtAreaW `div` 3) txtEdPos}
            | widthSum txtEdOff txtEdPos txtEdWidths >= txtAreaW = s{txtEdOff =
                let i = calcMinOff txtEdWidths txtAreaW $ VU.length txtEdWidths in
                if i < txtEdPos then i
                else calcMinOff txtEdWidths (txtAreaW - txtAreaW `div` 3) txtEdPos }
            | otherwise = s

{- s{txtEdOff = min (min (calcMinOff txtEdWidths txtAreaW $ VU.length txtEdWidths) txtEdPos) $
                               until (\off -> widthSum off txtEdPos txtEdWidths < txtAreaW) (DeltaOffset +) txtEdOff}
-}

    mbThreadId <- newMonadIORef Nothing
    d <- newMonadIORef TextEditStruct   { txtEdText = T.empty
                                        , txtEdOnChanged = \_ -> return ()
                                        , txtEdAtEnd = \_ -> return ()
                                        , txtEdVerif = \_ -> return True
--                                        , txtEdCharVerif = \_ -> return True
                                        , txtEdSetText = \_ -> return True -- tmp. set to onSetText below
                                        }
    state <- newMonadIORef $ adjustOffset TextEditState { txtEdPos = 0
                                                        , txtEdSel = -1
                                                        , txtEdOff = 0
                                                        , txtEdWidths = VU.empty
                                                        }

    let mkCharWidth :: MonadIO m => Char -> m Coord
        mkCharWidth = fmap xV2 . P.strSize fnt . pure

        mkWidthsVector :: MonadIO m => T.Text -> m (VU.Vector Coord)
        mkWidthsVector = fmap VU.fromList . mapM mkCharWidth . T.unpack

        posSelArrange' pos sel | (sel < 0) || (pos < sel) = (pos,sel)
                               |  otherwise = (sel,pos)

        posSelArrange TextEditState{..} = posSelArrange' txtEdPos txtEdSel

        onSetText :: MonadIO m => T.Text -> m Bool
        onSetText txt = do -- set into txtEdSetText
            s <- readMonadIORef state
            textUpdate' s txt 0 (-1)

        textUpdate' :: MonadIO m => TextEditState -> T.Text -> Int -> Int -> m Bool
        textUpdate' s@TextEditState{..} ins from to' = do
            t@TextEditStruct{..} <- readMonadIORef d
            let oldLn = T.length txtEdText
                to | to' < 0 = oldLn
                   | otherwise = to'
                maxInsert = textEditMaxChar - oldLn + to - from
                txt = T.take maxInsert $ T.filter textEditCharFilter ins
            if T.null ins || not (T.null txt) then
                let txt2 = T.concat [T.take from txtEdText, txt,  T.drop to txtEdText] in
                if txt2 /= txtEdText then do
                    verified <- txtEdVerif txt2
                    if verified then do
                        writeMonadIORef d t{txtEdText=txt2}
                        insV <- mkWidthsVector txt
                        let v = txtEdWidths
                        writeMonadIORef state $ adjustOffset s{
                            txtEdWidths= VU.concat [VU.slice 0 from v,insV,VU.slice to (VU.length v - to) v]
                            ,txtEdPos = from + T.length txt, txtEdSel= -1}
                        txtEdOnChanged txt2
                        return True
                    else return False
                else return False
            else return False

        textUpdate :: MonadIO m => T.Text -> m Bool
        textUpdate ins = do
            s@TextEditState{..} <- readMonadIORef state
            let (pos,sel) = posSelArrange s
                to | sel < 0 = pos
                   | otherwise = sel
            textUpdate' s ins pos to

        calcCharsVisible :: VU.Vector Coord -> Int -> Int
        calcCharsVisible v off = let vl = VU.length v
                                     go w i | i >= vl = i
                                            | otherwise = let w' = w + v VU.! i in
                                                          if w' >= txtAreaW then i else go w' (i+1)
                                 in go 0 off - off

        -- Coord from left of text area.
        coord2ix :: TextEditState -> Coord -> Int
        coord2ix TextEditState{..} x
            | x<=0 || x>=txtAreaW = -1
            | otherwise = let maxI = VU.length txtEdWidths
                              go r i | i == maxI = i
                                     | otherwise = let r' = r + txtEdWidths VU.! i
                                                   in if r' >= x then i else go r' $ i + 1
                              n = go (-3) txtEdOff
                          in {- if n == txtEdPos then -1 else -} n

        doMove :: MonadIO m => Widget -> (TextEditState -> (Int,Int)) -> m ()
        doMove widget f = do
            s <- readMonadIORef state
            let (pos,sel) = f s
            when (pos /= txtEdPos s || sel /= txtEdSel s) $ do
                writeMonadIORef state $ adjustOffset s{txtEdPos=pos, txtEdSel=sel}
                markWidgetForRedraw widget

        doMouse :: MonadIO m => Widget -> Coord -> (Int -> TextEditState -> (Int,Int)) -> m ()
        doMouse widget x f = do
            s@TextEditState{..} <- readMonadIORef state
            let pos = coord2ix s $ x - PaddingX
            --when (pos>=0) $
            doMove widget $ f pos

        copyToClipboard :: MonadIO m => m (TextEditState,Int,Int)
        copyToClipboard = do s@TextEditState{..} <- readMonadIORef state
                             if txtEdSel>=0 then do
                                t <- txtEdText <$> readMonadIORef d
                                let (from,to) = posSelArrange s
                                SDL.setClipboardText $ T.take (to - from) $ T.drop from t
                                return (s,from,to)
                             else return (s,-1,-1)

        cutToClipboard :: MonadIO m => Widget -> m ()
        cutToClipboard widget = do (s,from,to) <- copyToClipboard
                                   when (from>=0) $ whenM (textUpdate' s T.empty from to) $
                                        markWidgetForRedraw widget

        pasteFromClipboard :: MonadIO m => Widget -> m ()
        pasteFromClipboard widget = whenM (SDL.hasClipboardText) $
                                        whenM (SDL.getClipboardText >>= textUpdate) $
                                            markWidgetForRedraw widget
        atEnd :: MonadIO m => m ()
        atEnd = do
            TextEditStruct{..} <- readMonadIORef d
            txtEdAtEnd txtEdText

        stopThrd :: MonadIO m => m ()
        stopThrd = do
            mb <- readMonadIORef mbThreadId
            whenIsJust mb $ \tId -> do
                liftIO $ killThread tId
                writeMonadIORef mbThreadId Nothing

    blink <- newMonadIORef True
    modifyMonadIORef' d $ \s -> s{txtEdSetText= onSetText}
    win <- getWidgetWindow parent
    gui <- getWindowGui win
    blikPipe <- newGuiPipe gui $ \ _ (_ :: V.Vector Int) -> return ()
    void $ onSetText textEditText
    mkWidget textEditFlags
            (fromMaybe (formItemsMargin skin) $ formItemMargin textEditFormItemDef)
            (TextEditData d) parent fns{
        onCreate = \widget -> do
            onCreate fns widget
            replaceGuiPipeHandler gui blikPipe $ \ _ (_ :: V.Vector Int) -> do
                                                         modifyMonadIORef' blink not
                                                         markWidgetForRedraw widget
        ,onDestroy = \ _widget -> stopThrd >> delGuiPipe gui blikPipe
        ,onGainedKeyboardFocus = \_widget -> do
            stopThrd
            tId <- liftIO $ forkIO $ forever $ do
                        threadDelay 500000 -- 0.5 s
                        void $ sendToGuiPipe blikPipe (V.empty :: V.Vector Int)
            writeMonadIORef mbThreadId $ Just tId
        ,onLostKeyboardFocus = \widget -> stopThrd >> markWidgetForRedraw widget >> atEnd

        ,onMouseMotion = \widget btnsLst (P (V2 x _)) _relMv ->
            when (SDL.ButtonLeft `elem` btnsLst) $
                doMouse widget x $ \pos TextEditState{..} ->
                    let sel = if txtEdSel<0 then txtEdPos else txtEdSel in
                    if | pos >= 0 -> (pos,sel)
                       | x < PaddingX -> (0,sel)
                       | x > (PaddingX + txtAreaW) -> (VU.length txtEdWidths,sel)
                       | otherwise -> (txtEdPos,txtEdSel)

        ,onMouseButton = \widget motion mouseButton _clicks (P (V2 x _)) ->
            when ((motion==SDL.Pressed) && (mouseButton == SDL.ButtonLeft)) $ do
                setWidgetFocus widget
                doMouse widget x $ \pos TextEditState{..} ->
                    if pos>=0 then (pos,-1) else (txtEdPos,txtEdSel)
        ,onKeyboard = \widget motion _repeated keycode km -> when (motion==SDL.Pressed) $ do
--            liftIO $ putStrLn $ concat ["textEdit.onKeyboard "]
            let shiftCtrlAlt@ShiftCtrlAlt{isShift=isS,isCtrl=isC,isAlt=isA} = getShftCtrlAlt km
            if isEnterKey keycode && shiftCtrlAlt == ShiftCtrlAlt False False False then do
                n <- findNextTabbedWidget widget
                if n /= widget then setWidgetFocus n else atEnd
            else case keycode of
                    SDL.KeycodeA | not isS && isC && not isA -> doMove widget $ \ TextEditState{..} ->
                        let ln = VU.length txtEdWidths in
                        if ln>0 then (0,VU.length txtEdWidths) else (0,-1)

                    SDL.KeycodeHome | not isS && not isC && not isA -> doMove widget $ \ TextEditState{..} ->
                        (0,-1)
                    SDL.KeycodeEnd | not isS && not isC && not isA -> doMove widget $ \ TextEditState{..} ->
                        (VU.length txtEdWidths,-1)
                    SDL.KeycodeLeft | not isS && not isC && not isA -> doMove widget $ \ TextEditState{..} ->
                        (max 0 $ txtEdPos - 1,-1)
                    SDL.KeycodeRight | not isS && not isC && not isA  -> doMove widget $ \ TextEditState{..} ->
                        (min (VU.length txtEdWidths) $ txtEdPos + 1,-1)

                    SDL.KeycodeHome | isS && not isC && not isA -> doMove widget $ \ TextEditState{..} ->
                        (0,if txtEdSel<0 then txtEdPos else txtEdSel)
                    SDL.KeycodeEnd | isS && not isC && not isA -> doMove widget $ \ TextEditState{..} ->
                        (VU.length txtEdWidths,if txtEdSel<0 then txtEdPos else txtEdSel)
                    SDL.KeycodeLeft | isS && not isC && not isA -> doMove widget $ \ TextEditState{..} ->
                        (max 0 $ txtEdPos - 1,if txtEdSel<0 then txtEdPos else txtEdSel)
                    SDL.KeycodeRight | isS && not isC && not isA -> doMove widget $ \ TextEditState{..} ->
                        (min (VU.length txtEdWidths) $ txtEdPos + 1,if txtEdSel<0 then txtEdPos else txtEdSel)

                    SDL.KeycodeC | not isS && isC && not isA -> void copyToClipboard
                    SDL.KeycodeInsert | not isS && isC && not isA -> void copyToClipboard
                    SDL.KeycodeX | not isS && isC && not isA -> cutToClipboard widget
                    SDL.KeycodeDelete | not isS && isC && not isA -> cutToClipboard widget
                    SDL.KeycodeV | not isS && isC && not isA -> pasteFromClipboard widget
                    SDL.KeycodeInsert | isS && not isC && not isA -> pasteFromClipboard widget
                    SDL.KeycodeDelete | not isA -> do
--                        liftIO $ putStrLn "textEdit.KeycodeDelete"
                        s@TextEditState{..} <- readMonadIORef state
                        when (txtEdPos /= VU.length txtEdWidths) $
                            whenM (textUpdate' s T.empty txtEdPos (txtEdPos+1)) $
                                markWidgetForRedraw widget
                    SDL.KeycodeBackspace | not isA -> do
                        s@TextEditState{..} <- readMonadIORef state
                        when (txtEdPos>0) $ whenM (textUpdate' s T.empty (txtEdPos-1) txtEdPos) $
                            markWidgetForRedraw widget
                    _ -> return ()
        ,onTextInput = \widget txt -> whenM (textUpdate txt) $ markWidgetForRedraw widget
        ,onDraw= \widget -> do
                fl <- getWidgetFlags widget
                r@(SDL.Rectangle p0 (V2 fullW fullH)) <- getVisibleRect widget
                drawRoundFrame (bkColor skin) (borderColor skin) (windowBkColor skin) r
                txt <- txtEdText <$> readMonadIORef d
                let p1 = p0 .+^ V2 PaddingX PaddingY
                s@TextEditState{..} <- readMonadIORef state
                if (fl .&. (WidgetEnable .|. WidgetFocused)) == (WidgetEnable .|. WidgetFocused) then do
                    let chrsVisible = calcCharsVisible txtEdWidths txtEdOff
                        txt' = T.drop txtEdOff txt
                        rightOver = T.length txt' > chrsVisible
                        txt1 = T.take chrsVisible txt'
                        drawUnselected = drawTextOpaque fnt (windowFgColor skin) (windowBkColor skin)
                        drawSemiBorder x = do
                            let outSpan = 5
                                lineLn = 3
                            setColor (windowBkColor skin)
                            drawLine (P (V2 x outSpan)) (P (V2 x (outSpan + lineLn)))
                            drawLine (P (V2 x (fullH - outSpan))) (P (V2 x (fullH - outSpan - lineLn -1)))
                    if txtEdSel < 0 then do
                        drawUnselected p1 txt1
                        blinNow <- readMonadIORef blink
                        when blinNow $ do
                            setColor (windowFgColor skin)
                            let x = PaddingX + widthSum txtEdOff txtEdPos txtEdWidths
                            drawLine (P (V2 x 3)) (P (V2 x (fullH - 4)))
                    else do
                        let (selL,selR) = posSelArrange s
                            visibleSelL = max txtEdOff selL
                            visibleSelR = max txtEdOff selR
                            l = visibleSelL - txtEdOff
                            m = visibleSelR - visibleSelL
                            (txtL,txtRest) = T.splitAt l txt1
                            (txtSel,txtR) = T.splitAt m txtRest
                            p2 = p1 .+^ V2 (widthSum txtEdOff visibleSelL txtEdWidths) 0
                            p3 = p2 .+^ V2 (widthSum visibleSelL visibleSelR txtEdWidths) 0
                        drawUnselected p1 txtL
                        drawTextOpaque fnt (decoreFgColor $ selectedDecore skin)
                            (decoreBkColor $ selectedDecore skin) p2 txtSel
                        drawUnselected p3 txtR
                    when (txtEdOff>0) $ drawSemiBorder 0
                    when rightOver $ drawSemiBorder (fullW - 1)
                else let chrsVisible = calcCharsVisible txtEdWidths 0
                         txt1 = T.take chrsVisible txt
                         colorF = if (fl .&. WidgetEnable) == WidgetEnable then windowFgColor
                                  else windowDisabledColor
                     in drawTextOpaque fnt (colorF skin) (windowBkColor skin) p1 txt1
                                     }