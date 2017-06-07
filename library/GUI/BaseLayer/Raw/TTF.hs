{-# LANGUAGE RecordWildCards #-}
module GUI.BaseLayer.Raw.TTF(
    GuiFontStyle(..),getFontStyle,setFontStyle,setFontStyleIfNeed
    ) where

import Data.Bits
import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import SDL.TTF.FFI (TTFFont)
import qualified SDL.TTF.FFI as FFI

data GuiFontStyle = GuiFontStyle { fontBold          :: Bool
                                 , fontItalic        :: Bool
                                 , fontUnderline     :: Bool
                                 , fontStrikethrough :: Bool
                                 }
                                 deriving ( Eq, Ord, Show, Read )

instance Default GuiFontStyle where
    def = GuiFontStyle False False False False

getFontStyle :: MonadIO m => TTFFont     -- ^ Font
             -> m GuiFontStyle -- ^ Current style bitmask of the font.
getFontStyle fontPtr = do
    w <- liftIO $ FFI.getFontStyle fontPtr
    return   GuiFontStyle { fontBold = (w .&. 1) /= 0
                          , fontItalic = (w .&. 2) /= 0
                          , fontUnderline = (w .&. 4) /= 0
                          , fontStrikethrough = (w .&. 8) /= 0
                          }

setFontStyle :: MonadIO m => TTFFont     -- ^ Font
             -> GuiFontStyle    -- ^ The style as a bitmask
             -> m ()
setFontStyle fontPtr GuiFontStyle{..} =
    liftIO $ FFI.setFontStyle fontPtr (
        (if fontBold then 1 else 0) .|.
        (if fontItalic then 2 else 0) .|.
        (if fontUnderline then 4 else 0) .|.
        (if fontStrikethrough then 8 else 0) )

setFontStyleIfNeed :: MonadIO m => TTFFont     -- ^ Font
                                -> GuiFontStyle    -- ^ The style as a bitmask
                                -> m ()
setFontStyleIfNeed fontPtr style = do
    old <- getFontStyle fontPtr
    when (old /= style) $ setFontStyle fontPtr style