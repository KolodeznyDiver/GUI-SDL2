{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:      GUI.Widget.Button
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <kolodeznydiver@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Несколько разных виджетов - кнопок.

module GUI.Widget.Button(
    -- * Общий для нескольких типов кнопок тип состояния 'TextureButtonData'.
    TextureButtonData
    -- ** @textureButton@.
    -- Кнопка textureButton отображает только картинку (текстуру) произвольного происхождения,
    -- зато она может отображать часть картинки
    -- (колонки) в зависимости от состояния : мышь над ней или нет, нажата, disabled.
    -- К тому же картинка может быть разбита на ряды. Какой ряд отображать устанавливается программно.
    ,TextureButtonDef(..),textureButton
    -- ** @pictureButton@.
    -- Вариант предыдущей кнопки для которой текстура загружается из ресурса (из файла).
    ,PictureButtonDef(..),pictureButton
    -- ** @pictureWidget@.
    -- Упрощённый в настройке вариант кнопки, отображающий статическую картинку.
    ,PictureWidgetDef(..),pictureWidget
    -- * Общий для нескольких типов кнопок тип состояния 'ButtonData'.
    ,ButtonData
    -- * @button@
    -- Основная кнопка с возможностью отображения м картинки и текста (возможно, многострочного).
    ,ButtonPicture(..),ButtonDef(..),button
    -- * @buttonWithTriangle@.
    -- Кнопка с изображением повёрнутого в одну из 4-х сторон треугольника.
    ,ButtonWithTriangleType(..),ButtonWithTriangleDef(..),buttonWithTriangle
    -- * Вспомогательные функции.
    ,getButtonDecoreState,drawButtonFrame
    ) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.Bits
import Data.IORef
import Data.Maybe
import qualified SDL
import SDL.Vect
import Control.Monad.Extra (whenJust)
import Data.Default
import GUI
import qualified GUI.BaseLayer.Primitives as P
import GUI.Widget.Handlers
import GUI.Utils.TextWrap

-- | Параметры настройки кнопки с картинкой самого общего вида.
data TextureButtonDef = TextureButtonDef {
    buttonFormItemDef  :: FormItemWidgetDef -- ^ Общие настройки для всех виджетов для форм
                                            -- в настоящий момент только margin's.
  , buttonSize      :: GuiSize -- ^ размер без полей. Он же равен или кратен размеру картинки.
  , buttonFlags     :: WidgetFlags -- ^ Флаги базового виджета.
  , buttonTexture   :: SDL.Texture -- ^ текстура созданная заранее
  , buttonTextureOwned :: Bool -- ^ Если True, текстура будет удалена при закрытии виджета.
  , buttonMouseInPictIx :: Maybe Int  -- ^ если задано, то колонка в текстуре (часть текстуры)
                                      -- отображаемая когда указатель над кнопкой. \>0.
                                      -- 0 - колонка для обычного отображения.
  , buttonPressedPictIx ::  Maybe Int -- ^ если задано, то колонка в текстуре
                                      -- отображаемая когда указатель нажат над кнопкой.
  , buttonDisabledPictIx :: Maybe Int -- ^ если задано, то колонка в текстуре
                                      -- отображаемая когда виджет в состоянии disabled
                                      -- (его флаг @WidgetEnable@ сброшен, см. @enableWidget@).
  , buttonInitPictRow :: Int -- ^  начальное отображение ряда в текстуре.
                                            }

instance Default TextureButtonDef where
    def = TextureButtonDef  { buttonFormItemDef = def
                            , buttonSize = zero
                            , buttonFlags = WidgetVisible .|. WidgetEnable
                            , buttonTexture = undefined
                            , buttonTextureOwned = False
                            , buttonMouseInPictIx = Nothing
                            , buttonPressedPictIx = Nothing
                            , buttonDisabledPictIx = Nothing
                            , buttonInitPictRow = 0
                            }

-- | ОБщий тип для нескольких кнопок. Создаётся при создании кнопки,
-- экспортируется только левая часть.
data TextureButtonData = TextureButtonData { txtrbttnOnClick :: IORef NoArgAction
                                           , txtrbttnPictRow :: IORef Int
                                           , txtrbttnMouseState :: IORef WidgetMouseState
                                           }

-- | Экземпляры этого класса - виджеты, которые могут выполнить действие в ответ на клик.
instance Clickable (GuiWidget TextureButtonData) where
    onClick w a = writeMonadIORef (txtrbttnOnClick $ widgetData w) $ NoArgAction a

instance RowNumProperty (GuiWidget TextureButtonData) where
    setRowNum (GuiWidget widget TextureButtonData{..}) newV = do
        oldV <- readMonadIORef txtrbttnPictRow
        when (oldV /= newV) $ do
            writeMonadIORef txtrbttnPictRow newV
            markWidgetForRedraw widget
    getRowNum = readMonadIORef . txtrbttnPictRow . widgetData

instance MouseStateProperty (GuiWidget TextureButtonData) where
    getMouseState = readMonadIORef . txtrbttnMouseState . widgetData

-- | Универсальная кнопка с текстурой.
textureButton :: MonadIO m =>
                 TextureButtonDef ->  -- ^ Параметры виджета.
                 Widget ->  -- ^ Будующий предок в дереве виджетов.
                 Skin -> -- ^ Skin.
                 m (GuiWidget TextureButtonData)
textureButton TextureButtonDef{..} parent skin = do
    MouseAnimatedClickableHndlr
            { mouseAnimatedClickableMouseState = mouseState
            , mouseAnimatedClickableAction = onCLick'
            , mouseAnimatedClickableFs = fns
            } <- noChildrenClickableHndlr buttonSize (\_ _ _ -> return ())
    rowRf <- newMonadIORef buttonInitPictRow
    mkWidget buttonFlags
            (fromMaybe (formItemsMargin skin) $ formItemMargin buttonFormItemDef)
            (TextureButtonData onCLick' rowRf mouseState) parent fns{
            onDestroy = \_ -> when buttonTextureOwned $ SDL.destroyTexture buttonTexture
            ,onDraw= \widget -> do
                ena <- allWidgetFlags widget WidgetEnable
                ms  <- readMonadIORef mouseState
                row <- readMonadIORef rowRf
                let ix = if | not ena, Just i <- buttonDisabledPictIx -> i
                            | ms == WidgetMousePressed, Just i <- buttonPressedPictIx -> i
                            | ms == WidgetMouseIn, Just i <- buttonMouseInPictIx -> i
                            | otherwise -> 0
                    (V2 w h) = buttonSize
                    srcP = P (V2 (fromIntegral ix * w) (fromIntegral row * h))
                (SDL.Rectangle p0 _) <- getVisibleRect widget
{-                setColor $ rgb 255 0 0
                drawRect $ shrinkRect' 2 r
                (CanvasRecord renderer _ canvOff) <- ask
                let clip = SDL.rendererClipRect renderer
                clipR <- SDL.get clip
                liftIO $ putStrLn $ concat ["textureButton onDraw ix=",show ix,"  row=", show row,
                   " r=", rectToBriefStr r, "  canvOff=", show canvOff, "  clipR", show clipR] -}
                drawTexturePartial buttonTexture (SDL.Rectangle srcP buttonSize) p0
                                                        }
--------------------------------------------------

-- | Параметры настройки кнопки с картинкой из ресурса.
data PictureButtonDef = PictureButtonDef    {
      pictBtnFormItemDef  :: FormItemWidgetDef -- ^ Общие настройки для всех виджетов для форм
                                               -- в настоящий момент только margin's.
    , pictBtnFlags     :: WidgetFlags -- ^ флаги базового виджета.
    , pictBtnPicture   :: T.Text -- ^ Имя графического файла с картинкой.
    , pictBtnMouseInPictIx :: Maybe Int -- ^ если задано, то колонка в текстуре (часть текстуры)
                                        -- отображаемая когда указатель над кнопкой. \>0.
                                        -- 0 - колонка для обычного отображения.
    , pictBtnPressedPictIx ::  Maybe Int -- ^ если задано, то колонка в текстуре
                                         -- отображаемая когда указатель нажат над кнопкой.
    , pictBtnDisabledPictIx :: Maybe Int -- ^ если задано, то колонка в текстуре
                                         -- отображаемая когда виджет в состоянии disabled
                                         -- (его флаг @WidgetEnable@ сброшен, см. @enableWidget@).
    , pictBtnMaxRow :: Int -- ^ Кол-во рядов на которые следует разбить картинку.
    , pictBtnInitPictRow :: Int  -- ^  начальное отображение ряда в текстуре.
                                            }

instance Default PictureButtonDef where
    def = PictureButtonDef { pictBtnFormItemDef = def
                           , pictBtnFlags = WidgetVisible .|. WidgetEnable
                           , pictBtnPicture = T.empty
                           , pictBtnMouseInPictIx = Nothing
                           , pictBtnPressedPictIx = Nothing
                           , pictBtnDisabledPictIx = Nothing
                           , pictBtnMaxRow = 1
                           , pictBtnInitPictRow = 0
                           }

-- | Кнопка с картинкой из ресурса.
pictureButton :: MonadIO m => PictureButtonDef ->  -- ^ Параметры виджета.
                              Widget ->  -- ^ Будующий предок в дереве виджетов.
                              Skin -> -- ^ Skin.
                              m (GuiWidget TextureButtonData)
pictureButton PictureButtonDef{..} parent skin = do
    let incIfJust (Just _) prev' = succ prev'
        incIfJust _ prev' = prev'
        cCols = incIfJust pictBtnMouseInPictIx $ incIfJust pictBtnPressedPictIx $ incIfJust pictBtnDisabledPictIx 1
    texture <- runProxyCanvas parent $ getTexture pictBtnPicture
    (V2 tW tH) <- P.getTextureSize texture
    textureButton TextureButtonDef{ buttonFormItemDef = pictBtnFormItemDef
                                 , buttonSize = V2 (tW `div` cCols) (tH `div` pictBtnMaxRow)
                                 , buttonFlags = pictBtnFlags
                                 , buttonTexture = texture
                                 , buttonTextureOwned = False
                                 , buttonMouseInPictIx = pictBtnMouseInPictIx
                                 , buttonPressedPictIx = pictBtnPressedPictIx
                                 , buttonDisabledPictIx = pictBtnDisabledPictIx
                                 , buttonInitPictRow = pictBtnInitPictRow
                                                                             } parent skin

--------------------------------------------------------------------

-- | Параметры настройки кнопки без анимации. Она, впрочем, тоже отзовётся на щелчок,
-- но внешне это не отразится. Предполагается использовать как картики на форме.
-- Програмно менять ряд отображения, т.е. отображаемую часть картики так же можно
data PictureWidgetDef = PictureWidgetDef {
      pictFormItemDef  :: FormItemWidgetDef -- ^ Общие настройки для всех виджетов для форм
                                            -- в настоящий момент только margin's.
    , pictFlags     :: WidgetFlags -- ^ флаги базового виджета.
    , pictPicture   :: T.Text -- ^ Имя графического файла с картинкой.
    , pictMaxRow :: Int -- ^ Кол-во рядов на которые следует разбить картинку.
    , pictInitPictRow :: Int  -- ^  начальное отображение ряда в текстуре.
    }

instance Default PictureWidgetDef where
    def = PictureWidgetDef { pictFormItemDef = def
                           , pictFlags = WidgetVisible
                           , pictPicture = T.empty
                           , pictMaxRow = 1
                           , pictInitPictRow = 0
                           }

-- | Кнопка без анимации. Практически, область на форме для отображения картинки.
pictureWidget :: MonadIO m => PictureWidgetDef -> Widget -> Skin -> m (GuiWidget TextureButtonData)
pictureWidget PictureWidgetDef{..} = pictureButton PictureButtonDef {
                             pictBtnFormItemDef = pictFormItemDef
                           , pictBtnFlags = pictFlags
                           , pictBtnPicture = pictPicture
                           , pictBtnMouseInPictIx = Nothing
                           , pictBtnPressedPictIx = Nothing
                           , pictBtnDisabledPictIx = Nothing
                           , pictBtnMaxRow = pictMaxRow
                           , pictBtnInitPictRow = pictInitPictRow }


-------------------------------------- * @buttonWithTriangle@.
-- Кнопка с изображением повёрнутого в одну из 4-х сторон треугольника.

-- | Вид оформления для кнопки с треугольником.
data ButtonWithTriangleType =
--         ButtonWithTriangleScrollBar  -- ^ по 'Skin' -у для кнопок у ScrollBar-ов.
--       |
         ButtonWithTriangleInForm  -- ^ по 'Skin'-у для элементов форм.
       | ButtonWithTriangleUser { -- ^ Свои настройки цветов.
            btTriangleDecore :: ButtonDecore
                                }

-- | Параметры настройки кнопки с треугольником.
data ButtonWithTriangleDef = ButtonWithTriangleDef  {
      btTriangleFormItemDef  :: FormItemWidgetDef -- ^ Общие настройки для всех виджетов для форм,
                                                  -- в настоящий момент только margin's.
    , btTriangleFlags     :: WidgetFlags -- ^ Флаги базового виджета.
    , btTriangleOrientation :: Orientation -- ^ OrientationLeft | OrientationUp |
                                           -- OrientationRight | OrientationDown, см. "GUI.BaseLayer.Canvas".
    , btTriangleSize        :: GuiSize -- ^ Высота треугольника (несколько условно).
    , btTriangleType :: ButtonWithTriangleType -- ^ Вид оформления.
                                                    }

-- | Тип созданного виджета. Обычно используется как  @GuiWidget ButtonData@.
newtype ButtonData = ButtonData { buttonOnClick :: IORef NoArgAction
                                }

instance Clickable (GuiWidget ButtonData) where
    onClick w a = writeMonadIORef (buttonOnClick $ widgetData w) $ NoArgAction a

-- | Виджет - кнопка с треугольником.
-- Вообще то не так и нужен. Можно просто нарисовать картинку и использовать @pictureButton@.
-- В данной функции демонстрируется динамическое создание текстуры перед созданием собственно виджета.
buttonWithTriangle :: MonadIO m => ButtonWithTriangleDef -> -- ^ Параметры виджета.
                                   Widget -> -- ^ Будующий предок в дереве виджетов.
                                   Skin -> -- ^ Skin.
                                   m (GuiWidget ButtonData)
buttonWithTriangle ButtonWithTriangleDef{..} parent skin = do
    let (V2 btTriangleWidth btTriangleHeight) = btTriangleSize
        cPict = 4
        triangleW = round $ fromIntegral btTriangleWidth * (0.6 :: Double)
        btDecore = case btTriangleType of
--                                     ButtonWithTriangleScrollBar -> scrollBarArrow skin
                                     ButtonWithTriangleInForm -> formItemsButtons skin
                                     ButtonWithTriangleUser{..} -> btTriangleDecore
        draw1 x state = do
            let r=SDL.Rectangle (P (V2 x 0)) btTriangleSize
                decoreSt = getButtonDecoreState state btDecore
            drawButtonFrame decoreSt (btnDecoreBorder btDecore) r
            drawArrowTriangle btTriangleOrientation (decoreFgColor decoreSt) (rectCenter r) triangleW
            return $ x + btTriangleWidth
    texture <- runProxyCanvas parent $ do
        t <- createTargetTexture $ V2 (cPict*btTriangleWidth) btTriangleHeight
        withTargetTexture t $
            foldM_ draw1 0 [Just WidgetMouseOut,Just WidgetMouseIn,Just WidgetMousePressed,Nothing]
        return t
    (GuiWidget widgetSelf TextureButtonData{txtrbttnOnClick=onClk}) <- textureButton
                  def{ buttonFormItemDef = btTriangleFormItemDef
                     , buttonSize = btTriangleSize
                     , buttonTexture = texture
                     , buttonTextureOwned = True
                     , buttonMouseInPictIx = Just 1
                     , buttonPressedPictIx = Just 2
                     , buttonDisabledPictIx = Just 3
                     } parent skin
    return $ GuiWidget widgetSelf (ButtonData onClk)


------------------------------------------- * @button@
------- Основная кнопка с возможностью отображения м картинки и текста (возможно, многострочного).

-- | Местоположение картинки для @button@ и имя гравфайла.
data ButtonPicture = ButtonNoPicture  -- ^ Кнопка без картинки, только с текстом.
                   | ButtonLeftPicture T.Text -- ^ Картинка слева, указывается имя графического файла.
                   | ButtonBottomPicture T.Text  -- ^ Картинка снизу, указывается имя графического файла.
                   deriving (Show)

-- | Параметры настройки кнопки с текстом и картинкой.
data ButtonDef = ButtonDef  {
      btnFormItemDef  :: FormItemWidgetDef -- ^ Общие настройки для всех виджетов для форм
                                           -- в настоящий момент только margin's.
    , btnSize  :: GuiSize -- ^ Размер кнопки без полей. Может увеличиваться, в зависимости
                          -- от настроек @btnTextWrapMode@ и размера текста.
    , btnFlags :: WidgetFlags -- ^ флаги базового виджета.
    , btnPicture :: ButtonPicture -- ^ Местоположение картинки и имя гравфайла.
    , btnTextWrapMode :: TextWrapMode -- ^ Способ отображения надписи.
    , btnText  :: T.Text -- ^ Собственно надпись для кнопки.
    , btnUseBorder :: Bool -- ^ Рисовать ли рамку кнопки.
                            }
                            deriving (Show)

instance Default ButtonDef where
    def = ButtonDef { btnFormItemDef = def
                    , btnSize = zero
                    , btnFlags = WidgetVisible .|. WidgetEnable .|. WidgetFocusable .|. WidgetTabbed
                    , btnPicture = ButtonNoPicture
                    , btnTextWrapMode = def
                    , btnText = T.empty
                    , btnUseBorder = True
                    }

-- | Виджет - кнопка с текстом и картинкой.
button :: MonadIO m => ButtonDef ->  -- ^ Параметры виджета.
                       Widget -> -- ^ Будующий предок в дереве виджетов.
                       Skin -> -- ^ Skin.
                       m (GuiWidget ButtonData)
button ButtonDef{..} parent skin = do
    let (V2 btnW btnH) = btnSize
        prepareLabel r@(SDL.Rectangle _ (V2 rW rH)) =
            let wm  = case btnTextWrapMode of
                        TextNoWrap{..} -> TextNoWrap (textAreaMaxWidth + rW - btnW)
                        TextWrap{..} -> TextWrap (textAreaMaxHeight + rH - btnH) textAreaLineSpacing
            in prepareText parent DrawTextDef  { drawTextRect = r
                                                      , drawTextWrap = wm
                                                      , drawTextFontKey = "label"
                                                      , drawTextAlignment = AlignCenter
                                                      , drawTextText = btnText
                                                      }
        ButtonDecore{..} = formItemsButtons skin

    (inSize,mbPict,prep) <- case btnPicture of
        ButtonNoPicture -> do
            let r = shrinkRect (V2 MinInsideSpaceX MinInsideSpaceY) $ SDL.Rectangle zero btnSize
            p <- prepareLabel r
            return ( V2 (2*MinInsideSpaceX) (2*MinInsideSpaceY) +
                    unP (getRectRB $ drawTextRect $ preparedTextDef p), Nothing, p)
        (ButtonLeftPicture k) -> do
--            liftIO $ putStrLn "button.ButtonLeftPicture"
            texture <- runProxyCanvas parent $ getTexture k
--            liftIO $ putStrLn "button.ButtonLeftPicture after getTexture"
            (V2 textureW textureH) <- P.getTextureSize texture
            let btnH' = max (btnH - 2*MinInsideSpaceY) textureH
                r = SDL.Rectangle (P (V2 (textureW + 2*MinInsideSpaceX) MinInsideSpaceY))
                        (V2 (btnW - textureW - 3*MinInsideSpaceX) btnH')
{-            liftIO $ putStrLn $ concat ["button.ButtonLeftPicture textureW=",show textureW,
               " textureH=", show textureH, "  btnH'=", show btnH', " r=", rectToBriefStr r ] -}
            p <- prepareLabel r
            let SDL.Rectangle (P (V2 lx _)) (V2 lw lh) = drawTextRect $ preparedTextDef p
                maxH = max lh textureH
                textL = getLeftOfPreparedWrapText $ preparedText p
                textureL = max MinInsideSpaceX  (textL - textureW - MinInsideSpaceX )
            let texturePos = P (V2 textureL
                                   (MinInsideSpaceY + (maxH - textureH) `div` 2))
                resSz = V2 (lx + lw + MinInsideSpaceX) (maxH + 2*MinInsideSpaceY)
{-            liftIO $ putStrLn $ concat ["button.ButtonLeftPicture p=",show p, "   dtR=", rectToBriefStr dtR,
                "  maxH=", show maxH, " texturePos= ", show texturePos, "  resSz=",show resSz] -}
            return (resSz,Just (texture,texturePos),p)
        (ButtonBottomPicture k) -> do
            texture <- runProxyCanvas parent $ getTexture k
            (V2 textureW textureH) <- P.getTextureSize texture
            let btnW' = max (btnW - 2*MinInsideSpaceX) textureW
                r = SDL.Rectangle (P (V2 MinInsideSpaceX MinInsideSpaceY))
                        (V2 btnW' (btnH - textureH - {-3-}2*MinInsideSpaceY) )
            p <- prepareLabel r
            let SDL.Rectangle _ (V2 lw lh) = drawTextRect $ preparedTextDef p
                maxW = max lw textureW
            let texturePos = P (V2 (MinInsideSpaceX  + (maxW - textureW) `div` 2) ({-2*-}MinInsideSpaceY + lh))
            return (V2 (maxW + 2*MinInsideSpaceX) ({-3-}2*MinInsideSpaceY + lh + textureH),Just (texture,texturePos),p)
    MouseAnimatedClickableHndlr
                { mouseAnimatedClickableMouseState = mouseState
                , mouseAnimatedClickableAction = onCLick'
                , mouseAnimatedClickableFs = fns
                } <- noChildrenClickableHndlr inSize
                        (\widget pressed _ -> when pressed $ setWidgetFocus widget)
    mkWidget btnFlags
            (fromMaybe (formItemsMargin skin) $ formItemMargin btnFormItemDef)
            (ButtonData onCLick') parent fns{
            onDraw= \widget -> do
                fl <- getWidgetFlags widget
                let ena = (fl .&. WidgetEnable) /= WidgetNoFlags
                d <- if ena
                     then do
                        ms  <- readMonadIORef mouseState
                        case ms of
                            WidgetMouseOut -> return (if (fl .&. WidgetFocused) /= WidgetNoFlags
                                                      then btnDecoreFocused else btnDecoreOut)
                            WidgetMouseIn -> return btnDecoreIn
                            WidgetMousePressed -> return btnDecorePressed
                     else return btnDecoreDisabled
                r <- getVisibleRect widget
--                setColor $ V4 255 0 0 0
--                fillRect r
--                drawRoundFrame  (decoreBrdrColor d) (decoreBkColor d) r
--                liftIO $ putStrLn $ concat ["button.onDraw ", rectToBriefStr r]
                if btnUseBorder then
                     drawButtonFrame d btnDecoreBorder r
                else setColor (decoreBkColor d) >> fillRect r
                drawPreparedText prep (decoreFgColor d)
                    (textWrapModeToMbBkColor btnTextWrapMode skin $ decoreBkColor d)
                whenJust mbPict $ \(texture,texturePos) ->
                    if ena then drawTexture texture texturePos
                    else withTransparentTexture 40 texture $ drawTexture texture texturePos
--                when ((fl .&. WidgetFocused) /= WidgetNoFlags) $
--                    drawDotBorder 4 $ shrinkRect' 2 r
                                                                          }

------------------------------ * Вспомогательные функции.


getButtonDecoreState :: Maybe WidgetMouseState -> ButtonDecore -> DecoreState
getButtonDecoreState (Just WidgetMouseOut) = btnDecoreOut
getButtonDecoreState (Just WidgetMouseIn) = btnDecoreIn
getButtonDecoreState (Just WidgetMousePressed) = btnDecorePressed
getButtonDecoreState _ = btnDecoreDisabled

drawButtonFrame :: MonadIO m => DecoreState -> BtnBorderType -> GuiRect -> Canvas m ()
drawButtonFrame DecoreState{..} borderType r = do
    let borderWith = 1
    case borderType of
        BtnBorderRound brdrClr -> drawRoundFrame brdrClr decoreBkColor r
        BtnBorder3D brdrClr -> draw3DFrame (brdr3DLightColor brdrClr) (brdr3DDarkColor brdrClr)
                                    decoreBkColor borderWith r

