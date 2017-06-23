{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
module GUI.Widget.Button where

import Control.Monad
import Control.Monad.IO.Class
--import Control.Monad.Trans.Class
import qualified Data.Text as T
import Data.Bits
import Data.IORef
import Data.Maybe
import qualified SDL
import SDL.Vect
import Maybes (whenIsJust)
import Data.Default
import GUI
--import GUI.BaseLayer.Depend1.Geometry
import qualified GUI.BaseLayer.Primitives as P
import GUI.Widget.Handlers
import GUI.Utils.TextWrap
{-
import GUI.BaseLayer.Types --debug
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
-}
data TextureButtonDef = TextureButtonDef    { buttonFormItemDef  :: FormItemWidgetDef
                                            , buttonSize      :: GuiSize
                                            , buttonFlags     :: WidgetFlags
                                            , buttonTexture   :: SDL.Texture
                                            , buttonTextureOwned :: Bool
                                            , buttonMouseInPictIx :: Maybe Int
                                            , buttonPressedPictIx ::  Maybe Int
                                            , buttonDisabledPictIx :: Maybe Int
                                            , buttonInitPictRow :: Int
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

data TextureButtonData = TextureButtonData { txtrbttnOnClick :: IORef NoArgAction
                                           , txtrbttnPictRow :: IORef Int
                                           , txtrbttnMouseState :: IORef WidgetMouseState
                                           }

instance Clickable (GuiWidget TextureButtonData) where
    onClick w a = writeMonadIORef (txtrbttnOnClick $ getWidgetData w) $ NoArgAction a

instance RowNumProperty (GuiWidget TextureButtonData) where
    setRowNum (GuiWidget widget TextureButtonData{..}) newV = do
        oldV <- readMonadIORef txtrbttnPictRow
        when (oldV /= newV) $ do
            writeMonadIORef txtrbttnPictRow newV
            markWidgetForRedraw widget
    getRowNum = readMonadIORef . txtrbttnPictRow . getWidgetData

instance MouseStateProperty (GuiWidget TextureButtonData) where
    getMouseState = readMonadIORef . txtrbttnMouseState . getWidgetData

textureButton :: MonadIO m => TextureButtonDef -> Widget -> Skin -> m (GuiWidget TextureButtonData)
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

getButtonDecoreState :: Maybe WidgetMouseState -> ButtonDecore -> DecoreState
getButtonDecoreState (Just WidgetMouseOut) = btnDecoreOut
getButtonDecoreState (Just WidgetMouseIn) = btnDecoreIn
getButtonDecoreState (Just WidgetMousePressed) = btnDecorePressed
getButtonDecoreState _ = btnDecoreDisabled

drawButtonFrame :: MonadIO m => DecoreState -> BtnBorderType -> GuiColor -> GuiRect -> Canvas m ()
drawButtonFrame DecoreState{..} borderType externalColor r = do
    let borderWith = 1
    case borderType of
        BtnBorderRound brdrClr -> drawRoundFrame externalColor brdrClr decoreBkColor r
        BtnBorder3D brdrClr -> draw3DFrame (brdr3DLightColor brdrClr) (brdr3DDarkColor brdrClr)
                                    decoreBkColor borderWith r

data ButtonWithTriangleType = ButtonWithTriangleScrollBar
                            | ButtonWithTriangleInForm
                            | ButtonWithTriangleUser { btTriangleDecore :: ButtonDecore
                                                     , btTriangleExternalColor :: GuiColor
                                                     }

data ButtonWithTriangleDef = ButtonWithTriangleDef  { btTriangleFormItemDef  :: FormItemWidgetDef
                                                    , btTriangleFlags     :: WidgetFlags
                                                    , btTriangleOrientation :: Orientation
                                                    , btTriangleSize        :: GuiSize
                                                    , btTriangleType :: ButtonWithTriangleType
                                                    }
--                                                    deriving (Show)

newtype ButtonData = ButtonData { buttonOnClick :: IORef NoArgAction
                                }

instance Clickable (GuiWidget ButtonData) where
    onClick w a = writeMonadIORef (buttonOnClick $ getWidgetData w) $ NoArgAction a


buttonWithTriangle :: MonadIO m => ButtonWithTriangleDef -> Widget -> Skin -> m (GuiWidget ButtonData)
buttonWithTriangle ButtonWithTriangleDef{..} parent skin = do
    let (V2 btTriangleWidth btTriangleHeight) = btTriangleSize
        cPict = 4
        triangleW = round $ fromIntegral btTriangleWidth * (0.6 :: Double)
        (btDecore,externalColor) = case btTriangleType of
                                     ButtonWithTriangleScrollBar -> (scrollBarArrow skin,scrollBarColor skin)
                                     ButtonWithTriangleInForm -> (formItemsButtons skin,bkColor skin)
                                     ButtonWithTriangleUser{..} -> (btTriangleDecore,btTriangleExternalColor)
        draw1 x state = do
            let r=SDL.Rectangle (P (V2 x 0)) btTriangleSize
                decoreSt = getButtonDecoreState state btDecore
{-                c = case x `div` btTriangleWidth of
                        1 -> V4 255   0   0 0
                        2 -> V4 0   255   0 0
                        3 -> V4 0     0 255 0
                        _ -> V4 255 255   0 0
{-            setColor c
            fillRect r
            setColor $ V4 0 0 0 0
            drawLine (P (V2 x 0)) (P (V2 (x+btTriangleWidth) (btTriangleWidth)))
            drawLine (P (V2 x (btTriangleWidth))) (P (V2 (x+btTriangleWidth) 0)) -}
-}
--            drawRoundFrame btTriangleExternalColor (V4 0 0 255 0) c r
            --drawRoundFrame btTriangleExternalColor decoreBrdrColor decoreBkColor r
            drawButtonFrame decoreSt (btnDecoreBorder btDecore) externalColor r
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
------------------------------------------- button -----------------------------------------------------------------

data ButtonPicture = ButtonNoPicture
                   | ButtonLeftPicture T.Text
                   | ButtonBottomPicture T.Text
                   deriving (Show)

data ButtonDef = ButtonDef  { btnFormItemDef  :: FormItemWidgetDef
                            , btnSize      :: GuiSize
                            , btnFlags     :: WidgetFlags
                            , btnPicture :: ButtonPicture
                            , btnTextWrapMode :: TextWrapMode
                            , btnText  :: T.Text
                            }
                            deriving (Show)

instance Default ButtonDef where
    def = ButtonDef { btnFormItemDef = def
                    , btnSize = zero
                    , btnFlags = WidgetVisible .|. WidgetEnable .|. WidgetFocusable .|. WidgetTabbed
                    , btnPicture = ButtonNoPicture
                    , btnTextWrapMode = def
                    , btnText = T.empty
                    }

button :: MonadIO m => ButtonDef -> Widget -> Skin -> m (GuiWidget ButtonData)
button ButtonDef{..} parent skin = do
    let (V2 btnW btnH) = btnSize
        prepareLabel r@(SDL.Rectangle _ (V2 rW rH)) =
            let wm  = case btnTextWrapMode of
                        TextNoWrap{..} -> TextNoWrap (textAreaMaxWidth + rW - btnW)
                        TextWrap{..} -> TextWrap (textAreaMaxHeight + rH - btnH) textAreaLineSpacing
            in prepareText parent skin DrawTextDef  { drawTextRect = r
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
--            liftIO $ putStrLn $ concat ["button.ButtonLeftPicture textureW=",show textureW,
--               " textureH=", show textureH, "  btnH'=", show btnH', " r=", rectToBriefStr r ]
            p <- prepareLabel r
            let SDL.Rectangle (P (V2 lx _)) (V2 lw lh) = drawTextRect $ preparedTextDef p
                maxH = max lh textureH
            let texturePos = P (V2 MinInsideSpaceX (MinInsideSpaceY + (maxH - textureH) `div` 2))
                resSz = V2 (lx + lw + MinInsideSpaceX) (maxH + 2*MinInsideSpaceY)
--            liftIO $ putStrLn $ concat ["button.ButtonLeftPicture p=",show p, "   dtR=", rectToBriefStr dtR,
--                "  maxH=", show maxH, " texturePos= ", show texturePos, "  resSz=",show resSz]
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
                drawButtonFrame d btnDecoreBorder (bkColor skin) r
                drawPreparedText prep (decoreFgColor d)
                    (textWrapModeToMbBkColor btnTextWrapMode skin $ decoreBkColor d)
                whenIsJust mbPict $ \(texture,texturePos) ->
                    if ena then drawTexture texture texturePos
                    else withTransparentTexture 40 texture $ drawTexture texture texturePos
--                when ((fl .&. WidgetFocused) /= WidgetNoFlags) $
--                    drawDotBorder 4 $ shrinkRect' 2 r
                                                                          }

