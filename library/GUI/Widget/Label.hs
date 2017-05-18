{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
module GUI.Widget.Label(
    -- GUI.Util.TextWrap
    TextWrapMode(..),DrawTextDef(..),PreparedText(..)
    -- GUI.Widget.Label
    ,LabelDef(..),LabelData(..),label
    ) where

import Control.Monad
import Control.Monad.IO.Class
--import Control.Monad.Trans.Class
--import Maybes (whenIsJust)
import qualified Data.Text as T
import Data.Bits
import Data.IORef
import Data.Maybe
import Data.Default
import qualified SDL
import SDL.Vect
--import qualified SDL.TTF as TTF
import GUI
import GUI.Utils.TextWrap

data LabelDef = LabelDef    { labelFormItemDef  :: FormItemWidgetDef
                            , labelSize      :: GuiSize
                            , labelFlags     :: WidgetFlags
                            , labelAlignment :: Alignment
                            , labelFontKey :: T.Text
                            , labelColor :: Maybe GuiColor
                            , labelBkColor :: Maybe GuiColor
                            , labelWrapMode :: TextWrapMode
                            , labelText  :: T.Text
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

data LabelData = LabelData { labelPreparedText :: IORef PreparedText
                           , labelTextColor :: IORef GuiColor
                           }

instance TextProperty (GuiWidget LabelData) where
    setText (GuiWidget widget LabelData{..}) txt = do
        PreparedText{..} <- readMonadIORef labelPreparedText
        when (txt /= drawTextText preparedTextDef) $ do
            skin <- getSkinFromWidget widget
            p <- prepareText widget skin preparedTextDef{drawTextText=txt}
            writeMonadIORef labelPreparedText p
            markWidgetForRedraw widget
    getText (GuiWidget _ LabelData{..}) = (drawTextText . preparedTextDef) <$> readMonadIORef labelPreparedText


instance TextColorProperty (GuiWidget LabelData) where
    setTextColor (GuiWidget widget LabelData{..}) color = do
        oc <- readMonadIORef labelTextColor
        when (color /= oc) $ do
            writeMonadIORef labelTextColor color
            markWidgetForRedraw widget
    getTextColor (GuiWidget _ LabelData{..}) = readMonadIORef labelTextColor

label :: MonadIO m => LabelDef -> Widget -> Skin -> m (GuiWidget LabelData)
label LabelDef{..} parent skin = do
    p <- prepareText parent skin DrawTextDef  { drawTextRect = SDL.Rectangle zero labelSize
                                                , drawTextWrap = labelWrapMode
                                                , drawTextFontKey = labelFontKey
                                                , drawTextAlignment = labelAlignment
                                                , drawTextText = labelText
                                                                   }
    let insideSz = unP $ getRectRB $ drawTextRect $ preparedTextDef p
    prepRf <- newMonadIORef p
    colorRf <- newMonadIORef $ fromMaybe (foregroundColor skin) labelColor
    let bkgrndColor = fromMaybe (bkColor skin) labelBkColor
        bkTxtColor = textWrapModeToMbBkColor labelWrapMode skin bkgrndColor
        fns = noChildrenFns insideSz
    mkWidget labelFlags
            (fromMaybe (formItemsMargin skin) $ formItemMargin labelFormItemDef)
            (LabelData prepRf colorRf) parent fns{
        onDraw= \widget -> do
            ena <- allWidgetFlags widget WidgetEnable
            c <- if ena then readMonadIORef colorRf else return $ disableFgColor skin
            r <- getVisibleRect widget
--            liftIO $ putStrLn $ concat ["label.onDraw getVisibleRect=", rectToBriefStr r]
            setColor bkgrndColor
            fillRect r
            prep <- readMonadIORef prepRf
            drawPreparedText prep c bkTxtColor
--            setColor $ rgb 255 0 0
--            drawRect $ shrinkRect' 1 r
        ,onResizing= \widget newRect -> do
            onResizing fns widget newRect
            PreparedText{..} <- readMonadIORef prepRf
            when (sizeOfRect newRect /= sizeOfRect (drawTextRect preparedTextDef)) $ do
                r <- getWidgetCanvasRect widget
                prepareText widget skin preparedTextDef{drawTextRect=r} >>= writeMonadIORef prepRf
                                                }

