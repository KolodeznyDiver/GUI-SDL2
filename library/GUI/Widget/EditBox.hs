{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
-- |
-- Module:      GUI.Widget.EditBox
-- Copyright:   (c) 2017-2018 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Поле для редактирования текста.
-- Использует шрифт \"edit\" из таблицы шрифтов менеджера ресурсов.

module GUI.Widget.EditBox(
    -- * Типы поля для редактирования текста.
    EditBoxDef(..),EditBoxData
    -- * Функция создания виджета для редактирования текста.
    ,editBox
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
import Control.Monad.Extra (whenJust,whenM)
import Data.Default
import qualified SDL
import SDL.Vect
import qualified SDL.Font as FNT
import GUI
import qualified GUI.BaseLayer.Primitives as P
import GUI.Widget.Handlers

pattern PaddingX :: Coord
pattern PaddingX = 5
pattern PaddingY :: Coord
pattern PaddingY = 3

-- | Начальные настройки виджета для редактирования текста.
data EditBoxDef = EditBoxDef {
          editBoxFormItemDef  :: FormItemWidgetDef -- ^ Общие настройки для всех виджетов для форм,
                                                    -- в настоящий момент только margin's.
        , editBoxWidth   :: Coord -- ^ Ширина поля редактирования. Высота определяется размером шрифта
                                   -- и внутренними полями фиксированного размера.
        , editBoxFlags   :: WidgetFlags -- ^ Флаги базового виджета.
        , editBoxText    :: T.Text -- ^ Исходный текст.
        , editBoxMaxChar :: Int -- ^ максимально допустимое число символов.
        , editBoxCharFilter :: Char -> Bool -- ^ функция-фильтр допустимых для ввода символов.
                                             -- по умолчанию недопустимы пробельные символы кроме пробела.
                               }

instance Default EditBoxDef where
    def = EditBoxDef { editBoxFormItemDef = def
                     , editBoxWidth = 100
                     , editBoxFlags = WidgetVisible .|. WidgetEnable .|. WidgetFocusable .|. WidgetTabbed
                     , editBoxText = T.empty
                     , editBoxMaxChar = 100
                     , editBoxCharFilter = \c -> not (isSpace c) || c == ' '
                     }

-- | Не экспортируемый тип записи. Хранится по ссылке в 'EditBoxData'.
data EditBoxStruct = EditBoxStruct    { edBxText :: T.Text
                                      , edBxOnChanged :: forall m. MonadIO m => T.Text -> m ()
                                      , edBxAtEnd :: forall m. MonadIO m => T.Text -> m ()
                                      , edBxVerif :: forall m. MonadIO m => T.Text -> m Bool
--                                        , edBxCharVerif :: forall m. MonadIO m => Char -> m Bool
                                      , edBxSetText :: forall m. MonadIO m => T.Text -> m Bool
                                      }

-- | Тип созданного виджета. Обычно используется как  @GuiWidget EditBoxData@.
newtype EditBoxData = EditBoxData { getTxtEd :: IORef EditBoxStruct }

-- | Установка функции-обработчика на изменение текста.
instance Changeable (GuiWidget EditBoxData) T.Text where
    onChanged w a = modifyMonadIORef' (getTxtEd $ widgetData w) (\d -> d{edBxOnChanged= a})

-- | Установка функции-обработчика вызываемого когда поле редактирования теряет фокус или нажат Enter.
instance OnEnd (GuiWidget EditBoxData) T.Text where
    onEnd w a = modifyMonadIORef' (getTxtEd $ widgetData w) (\d -> d{edBxAtEnd= a})

-- | Установка предиката-верификатора. Он вызывается при любой ПОПЫТКЕ изменения текста.
-- если предикат вернёт False, текст не будет изменён.
instance Verifiable (GuiWidget EditBoxData) T.Text where
    setVerifier w a = modifyMonadIORef' (getTxtEd $ widgetData w) (\d -> d{edBxVerif= a})

{-
setEditBoxCharVerifier :: forall m. MonadIO m => GuiWidget EditBoxData ->
                            (forall n. MonadIO n => Char -> n Bool) -> m ()
setEditBoxCharVerifier w a = modifyMonadIORef' (getTxtEd $ widgetData w) (\d -> d{edBxCharVerif= a})
-}

-- | Установка и извлечение редактируемого текста.
instance TextProperty (GuiWidget EditBoxData) where
    setText (GuiWidget widget EditBoxData{..}) txt = do
        EditBoxStruct{..} <- readMonadIORef getTxtEd
        when (txt /= edBxText) $
            whenM (edBxSetText txt) $
                markWidgetForRedraw widget
    getText (GuiWidget _ EditBoxData{..}) = edBxText <$> readMonadIORef getTxtEd


-- no export
widthSum :: Int -> Int -> VU.Vector Coord -> Coord
widthSum from to | from < to = VU.sum . VU.slice from (to - from)
                 | otherwise = const 0
{-# INLINE widthSum #-}

-- internal for editBox
data EditBoxState = EditBoxState      { edBxPos :: Int -- Caret pos. or start selection
                                      , edBxSel :: Int -- stop selection or -1
                                      , edBxOff :: Int -- view area offset in Chars
                                      , edBxWidths :: VU.Vector Coord
                                      }

-- | Функция создания виджета для редактирования текста.
editBox :: MonadIO m => EditBoxDef -> -- ^ Параметры виджета.
                         Widget -> -- ^ Будующий предок в дереве виджетов.
                         Skin -> -- ^ Skin.
                         m (GuiWidget EditBoxData)
editBox EditBoxDef{..} parent skin = do
    fnt <- runProxyCanvas parent $ getFont "edit"
    fntHeight <- FNT.lineSkip fnt -- FNT.height fnt

    let allSz = V2 editBoxWidth (fntHeight + 2*PaddingY)
        fns = noChildrenFns allSz
        txtAreaW = editBoxWidth - 2*PaddingY

        calcMinOff :: VU.Vector Coord -> Coord -> Int -> Int
        calcMinOff v = go
            where go _ 0 = 0
                  go r i = let  i' = i - 1
                                r' = r - v VU.! i'
                           in if r' <= 0 then i else go r' i'

        adjustOffset :: EditBoxState -> EditBoxState
        adjustOffset s@EditBoxState{..}
            | (edBxPos < edBxOff) || ((edBxPos == edBxOff) && (edBxOff > 0)) =
                            s{edBxOff= calcMinOff edBxWidths (txtAreaW `div` 3) edBxPos}
            | widthSum edBxOff edBxPos edBxWidths >= txtAreaW = s{edBxOff =
                let i = calcMinOff edBxWidths txtAreaW $ VU.length edBxWidths in
                if i < edBxPos then i
                else calcMinOff edBxWidths (txtAreaW - txtAreaW `div` 3) edBxPos }
            | otherwise = s

{- s{edBxOff = min (min (calcMinOff edBxWidths txtAreaW $ VU.length edBxWidths) edBxPos) $
                               until (\off -> widthSum off edBxPos edBxWidths < txtAreaW) (DeltaOffset +) edBxOff}
-}

    mbThreadId <- newMonadIORef Nothing
    d <- newMonadIORef EditBoxStruct   { edBxText = T.empty
                                        , edBxOnChanged = \_ -> return ()
                                        , edBxAtEnd = \_ -> return ()
                                        , edBxVerif = \_ -> return True
--                                        , edBxCharVerif = \_ -> return True
                                        , edBxSetText = \_ -> return True -- tmp. set to onSetText below
                                        }
    state <- newMonadIORef $ adjustOffset EditBoxState { edBxPos = 0
                                                        , edBxSel = -1
                                                        , edBxOff = 0
                                                        , edBxWidths = VU.empty
                                                        }

    let mkCharWidth :: MonadIO m => Char -> m Coord
        mkCharWidth = fmap xV2 . P.textSize fnt . T.singleton

        mkWidthsVector :: MonadIO m => T.Text -> m (VU.Vector Coord)
        mkWidthsVector = fmap VU.fromList . mapM mkCharWidth . T.unpack

        posSelArrange' pos sel | (sel < 0) || (pos < sel) = (pos,sel)
                               |  otherwise = (sel,pos)

        posSelArrange EditBoxState{..} = posSelArrange' edBxPos edBxSel

        onSetText :: MonadIO m => T.Text -> m Bool
        onSetText txt = do -- set into edBxSetText
            s <- readMonadIORef state
            textUpdate' s txt 0 (-1)

        textUpdate' :: MonadIO m => EditBoxState -> T.Text -> Int -> Int -> m Bool
        textUpdate' s@EditBoxState{..} ins from to' = do
            t@EditBoxStruct{..} <- readMonadIORef d
            let oldLn = T.length edBxText
                to | to' < 0 = oldLn
                   | otherwise = to'
                maxInsert = editBoxMaxChar - oldLn + to - from
                txt = T.take maxInsert $ T.filter editBoxCharFilter ins
            if T.null ins || not (T.null txt) then
                let txt2 = T.concat [T.take from edBxText, txt,  T.drop to edBxText] in
                if txt2 /= edBxText then do
                    verified <- edBxVerif txt2
                    if verified then do
                        writeMonadIORef d t{edBxText=txt2}
                        insV <- mkWidthsVector txt
                        let v = edBxWidths
                        writeMonadIORef state $ adjustOffset s{
                            edBxWidths= VU.concat [VU.slice 0 from v,insV,VU.slice to (VU.length v - to) v]
                            ,edBxPos = from + T.length txt, edBxSel= -1}
                        edBxOnChanged txt2
                        return True
                    else return False
                else return False
            else return False

        textUpdate :: MonadIO m => T.Text -> m Bool
        textUpdate ins = do
            s@EditBoxState{..} <- readMonadIORef state
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
        coord2ix :: EditBoxState -> Coord -> Int
        coord2ix EditBoxState{..} x
            | x<=0 || x>=txtAreaW = -1
            | otherwise = let maxI = VU.length edBxWidths
                              go r i | i == maxI = i
                                     | otherwise = let r' = r + edBxWidths VU.! i
                                                   in if r' >= x then i else go r' $ i + 1
                              n = go (-3) edBxOff
                          in {- if n == edBxPos then -1 else -} n

        doMove :: MonadIO m => Widget -> (EditBoxState -> (Int,Int)) -> m ()
        doMove widget f = do
            s <- readMonadIORef state
            let (pos,sel) = f s
            when (pos /= edBxPos s || sel /= edBxSel s) $ do
                writeMonadIORef state $ adjustOffset s{edBxPos=pos, edBxSel=sel}
                markWidgetForRedraw widget

        doMouse :: MonadIO m => Widget -> Coord -> (Int -> EditBoxState -> (Int,Int)) -> m ()
        doMouse widget x f = do
            s@EditBoxState{..} <- readMonadIORef state
            let pos = coord2ix s $ x - PaddingX
            --when (pos>=0) $
            doMove widget $ f pos

        copyToClipboard :: MonadIO m => m (EditBoxState,Int,Int)
        copyToClipboard = do s@EditBoxState{..} <- readMonadIORef state
                             if edBxSel>=0 then do
                                t <- edBxText <$> readMonadIORef d
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
            EditBoxStruct{..} <- readMonadIORef d
            edBxAtEnd edBxText

        stopThrd :: MonadIO m => m ()
        stopThrd = do
            mb <- readMonadIORef mbThreadId
            whenJust mb $ \tId -> do
                liftIO $ killThread tId
                writeMonadIORef mbThreadId Nothing

    blink <- newMonadIORef True
    modifyMonadIORef' d $ \s -> s{edBxSetText= onSetText}
    win <- getWidgetWindow parent
    gui <- getWindowGui win
    blikPipe <- newGuiPipe gui $ \ _ (_ :: V.Vector Int) -> return ()
    void $ onSetText editBoxText
    mkFormWidget editBoxFormItemDef editBoxFlags skin id (EditBoxData d) parent fns{
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
                doMouse widget x $ \pos EditBoxState{..} ->
                    let sel = if edBxSel<0 then edBxPos else edBxSel in
                    if | pos >= 0 -> (pos,sel)
                       | x < PaddingX -> (0,sel)
                       | x > (PaddingX + txtAreaW) -> (VU.length edBxWidths,sel)
                       | otherwise -> (edBxPos,edBxSel)

        ,onMouseButton = \widget motion mouseButton _clicks (P (V2 x _)) ->
            when ((motion==SDL.Pressed) && (mouseButton == SDL.ButtonLeft)) $ do
--                setWidgetFocus widget
                doMouse widget x $ \pos EditBoxState{..} ->
                    if pos>=0 then (pos,-1) else (edBxPos,edBxSel)
        ,onKeyboard = \widget motion _repeated keycode km -> when (motion==SDL.Pressed) $ do
--            liftIO $ putStrLn $ concat ["editBox.onKeyboard "]
            let shiftCtrlAlt@ShiftCtrlAlt{isShift=isS,isCtrl=isC,isAlt=isA} = getShftCtrlAlt km
            if isEnterKey keycode && shiftCtrlAlt == ShiftCtrlAlt False False False then do
                n <- findNextTabbedWidget widget
                if n /= widget then setWidgetFocus n else atEnd
            else case keycode of
                    SDL.KeycodeA | not isS && isC && not isA -> doMove widget $ \ EditBoxState{..} ->
                        let ln = VU.length edBxWidths in
                        if ln>0 then (0,VU.length edBxWidths) else (0,-1)

                    SDL.KeycodeHome | not isS && not isC && not isA -> doMove widget $ \ EditBoxState{..} ->
                        (0,-1)
                    SDL.KeycodeEnd | not isS && not isC && not isA -> doMove widget $ \ EditBoxState{..} ->
                        (VU.length edBxWidths,-1)
                    SDL.KeycodeLeft | not isS && not isC && not isA -> doMove widget $ \ EditBoxState{..} ->
                        (max 0 $ edBxPos - 1,-1)
                    SDL.KeycodeRight | not isS && not isC && not isA  -> doMove widget $ \ EditBoxState{..} ->
                        (min (VU.length edBxWidths) $ edBxPos + 1,-1)

                    SDL.KeycodeHome | isS && not isC && not isA -> doMove widget $ \ EditBoxState{..} ->
                        (0,if edBxSel<0 then edBxPos else edBxSel)
                    SDL.KeycodeEnd | isS && not isC && not isA -> doMove widget $ \ EditBoxState{..} ->
                        (VU.length edBxWidths,if edBxSel<0 then edBxPos else edBxSel)
                    SDL.KeycodeLeft | isS && not isC && not isA -> doMove widget $ \ EditBoxState{..} ->
                        (max 0 $ edBxPos - 1,if edBxSel<0 then edBxPos else edBxSel)
                    SDL.KeycodeRight | isS && not isC && not isA -> doMove widget $ \ EditBoxState{..} ->
                        (min (VU.length edBxWidths) $ edBxPos + 1,if edBxSel<0 then edBxPos else edBxSel)

                    SDL.KeycodeC | not isS && isC && not isA -> void copyToClipboard
                    SDL.KeycodeInsert | not isS && isC && not isA -> void copyToClipboard
                    SDL.KeycodeX | not isS && isC && not isA -> cutToClipboard widget
                    SDL.KeycodeDelete | not isS && isC && not isA -> cutToClipboard widget
                    SDL.KeycodeV | not isS && isC && not isA -> pasteFromClipboard widget
                    SDL.KeycodeInsert | isS && not isC && not isA -> pasteFromClipboard widget
                    SDL.KeycodeDelete | not isA -> do
--                        liftIO $ putStrLn "editBox.KeycodeDelete"
                        s@EditBoxState{..} <- readMonadIORef state
                        when (edBxPos /= VU.length edBxWidths) $
                            whenM (textUpdate' s T.empty edBxPos (edBxPos+1)) $
                                markWidgetForRedraw widget
                    SDL.KeycodeBackspace | not isA -> do
                        s@EditBoxState{..} <- readMonadIORef state
                        when (edBxPos>0) $ whenM (textUpdate' s T.empty (edBxPos-1) edBxPos) $
                            markWidgetForRedraw widget
                    _ -> return ()
        ,onTextInput = \widget txt -> whenM (textUpdate txt) $ markWidgetForRedraw widget
        ,onDraw= \widget -> do
                fl <- getWidgetFlags widget
                r@(SDL.Rectangle p0 (V2 fullW fullH)) <- getVisibleRect widget
                drawRoundFrame (formBorderColor skin) (decoreBkColor (windowDecore skin)) r
                txt <- edBxText <$> readMonadIORef d
                let p1 = p0 .+^ V2 PaddingX PaddingY
                s@EditBoxState{..} <- readMonadIORef state
                let drawTxt _     _     _ _     0   _ = return ()
                    drawTxt fgClr bkClr p iFrom iLn t =
                        case T.uncons t of
                            Just (c,rest) -> drawCharOpaque fnt fgClr bkClr p c >>
                                                drawTxt fgClr bkClr
                                                    (p .+^ V2 (edBxWidths VU.! iFrom) 0)
                                                        (iFrom+1) (iLn-1) rest
                            _ -> return ()
                if (fl .&. (WidgetEnable .|. WidgetFocused)) == (WidgetEnable .|. WidgetFocused) then do
                    let chrsVisible = calcCharsVisible edBxWidths edBxOff
                        txt' = T.drop edBxOff txt
                        rightOver = T.length txt' > chrsVisible
                        txt1 = T.take chrsVisible txt'
                        drawUnselected = drawTxt (decoreFgColor (windowDecore skin))
                                                 (decoreBkColor (windowDecore skin))
                        drawSemiBorder x = do
                            let outSpan = 5
                                lineLn = 3
                            setColor (decoreBkColor (windowDecore skin))
                            drawLine (P (V2 x outSpan)) (P (V2 x (outSpan + lineLn)))
                            drawLine (P (V2 x (fullH - outSpan))) (P (V2 x (fullH - outSpan - lineLn -1)))
                    if edBxSel < 0 then do
                        drawUnselected p1 edBxOff chrsVisible txt1
                        blinNow <- readMonadIORef blink
                        when blinNow $ do
                            setColor (decoreFgColor (windowDecore skin))
                            let x = PaddingX + widthSum edBxOff edBxPos edBxWidths
                            drawLine (P (V2 x 3)) (P (V2 x (fullH - 4)))
                    else do
                        let (selL,selR) = posSelArrange s
                            visibleSelL = max edBxOff selL
                            visibleSelR = max edBxOff selR
                            l = visibleSelL - edBxOff
                            m = visibleSelR - visibleSelL
                            (txtL,txtRest) = T.splitAt l txt1
                            (txtSel,txtR) = T.splitAt m txtRest
                            p2 = p1 .+^ V2 (widthSum edBxOff visibleSelL edBxWidths) 0
                            p3 = p2 .+^ V2 (widthSum visibleSelL visibleSelR edBxWidths) 0
                        drawUnselected p1 edBxOff l txtL
                        drawTxt (decoreFgColor $ selectedDecore skin)
                                (decoreBkColor $ selectedDecore skin) p2
                                visibleSelL m txtSel
                        drawUnselected p3 visibleSelR 99999 txtR
                    when (edBxOff>0) $ drawSemiBorder 0
                    when rightOver $ drawSemiBorder (fullW - 1)
                else let chrsVisible = calcCharsVisible edBxWidths 0
                         txt1 = T.take chrsVisible txt
                         colorF = if (fl .&. WidgetEnable) == WidgetEnable then decoreFgColor . windowDecore
                                  else windowDisabledFgColor
                     in drawTxt (colorF skin) (decoreBkColor (windowDecore skin)) p1
                            0 chrsVisible txt1
                                     }