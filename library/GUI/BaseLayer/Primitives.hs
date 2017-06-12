{-# LANGUAGE RecordWildCards #-}
module GUI.BaseLayer.Primitives(
    DrawStrMode(..),toSDLV2,fromSDLV2,toSDLRect,mousePointToGuiPoint
    ,getTextureSize,drawTexture,drawTextureAligned,fromRawColor,toRawColor,strSize,getPixelFormat
    ,createTargetTexture,renderStr,renderStrDraft,renderStrOpaque,drawStr,drawStrDraft,drawStrOpaque
    ,withStateVar,withColor,withRendererColor,withRendererTarget,withRendererClipRect,withRendererViewport
    ,withUTF8,drawStrAligned
                     ) where

import Control.Monad.IO.Class (MonadIO,liftIO)
import GHC.IO.Encoding
import Data.StateVar
import Maybes (whenIsJust)
import qualified SDL
import qualified SDL.Raw as Raw
import SDL.Vect
import qualified SDL.TTF as TTF
import SDL.TTF.FFI (TTFFont)
import GUI.BaseLayer.Types
import GUI.BaseLayer.Geometry

data DrawStrMode = DrawStrFine
                 | DrawStrDraft
                 | DrawStrOpaque GuiColor
                 deriving (Eq, Show)

toSDLV2 :: V2 Coord -> V2 SDLCoord
toSDLV2 = fmap fromIntegral
{-# INLINE toSDLV2 #-}

fromSDLV2 :: V2 SDLCoord -> V2 Coord
fromSDLV2 = fmap fromIntegral
{-# INLINE fromSDLV2 #-}

toSDLRect :: GuiRect -> SDL.Rectangle SDLCoord
toSDLRect (SDL.Rectangle (SDL.P p) sz) = SDL.Rectangle (P $ toSDLV2 p) (toSDLV2 sz)
{-# INLINE toSDLRect #-}

mousePointToGuiPoint:: MousePoint -> GuiPoint
mousePointToGuiPoint = fmap fromIntegral
{-# INLINE mousePointToGuiPoint #-}

getTextureSize :: MonadIO m => SDL.Texture -> m GuiSize
getTextureSize = fmap (\ti -> fromIntegral <$> V2 (SDL.textureWidth ti) (SDL.textureHeight ti)) . SDL.queryTexture
{-# INLINE getTextureSize #-}

drawTexture :: MonadIO m => SDL.Renderer -> SDL.Texture -> GuiPoint -> m ()
drawTexture renderer texture pnt = do
    sz <- getTextureSize texture
    SDL.copy renderer texture Nothing $ Just $ toSDLRect $ SDL.Rectangle pnt sz

drawTextureAligned :: MonadIO m => SDL.Renderer -> SDL.Texture -> Alignment -> GuiRect -> m ()
drawTextureAligned renderer texture align rect = do
    sz <- getTextureSize texture
    SDL.copy renderer texture Nothing $ Just $ toSDLRect $ rectAlign align sz rect

fromRawColor :: Raw.Color -> GuiColor
fromRawColor Raw.Color{..} = V4 colorR colorG colorB colorA
{-# INLINE fromRawColor #-}

toRawColor :: GuiColor -> Raw.Color
toRawColor (V4 r g b a) = Raw.Color r g b a
{-# INLINE toRawColor #-}

getPixelFormat :: MonadIO m => SDL.Renderer -> m SDL.PixelFormat
getPixelFormat = fmap (head . SDL.rendererInfoTextureFormats) . SDL.getRendererInfo
{-# INLINE getPixelFormat #-}

createTargetTexture :: MonadIO m => SDL.Renderer -> GuiSize -> m SDL.Texture
createTargetTexture renderer sz = do    pf <- getPixelFormat renderer
                                        SDL.createTexture renderer pf SDL.TextureAccessTarget $ toSDLV2 sz
{-# INLINE createTargetTexture #-}

withStateVar ::  MonadIO m => StateVar a -> a -> m b -> m b
withStateVar sv a b = do {save <- get sv; sv $= a; r <- b; sv $= save; return r}
{-# INLINE withStateVar #-}

withColor :: MonadIO m => GuiCurDrawColor -> GuiColor -> m a -> m a
withColor = withStateVar
{-# INLINE withColor #-}

withRendererColor :: MonadIO m => SDL.Renderer -> GuiColor -> m a -> m a
withRendererColor renderer = withStateVar (SDL.rendererDrawColor renderer)
{-# INLINE withRendererColor #-}

withRendererTarget :: MonadIO m => SDL.Renderer -> SDL.Texture -> m a -> m a
withRendererTarget renderer t = withStateVar (SDL.rendererRenderTarget renderer) $ Just t
{-# INLINE withRendererTarget #-}

withRendererClipRect :: MonadIO m => SDL.Renderer -> GuiRect -> m a -> m a
withRendererClipRect renderer r = withStateVar (SDL.rendererClipRect renderer) $ Just $ toSDLRect r
{-# INLINE withRendererClipRect #-}

withRendererViewport :: MonadIO m => SDL.Renderer -> GuiRect -> m a -> m a
withRendererViewport renderer r = withStateVar (SDL.rendererViewport renderer) $ Just $ toSDLRect r
{-# INLINE withRendererViewport #-}

withUTF8 ::  MonadIO m => m a -> m a
withUTF8 f = do
    save <- liftIO (getForeignEncoding <* setForeignEncoding utf8)
    r <- f
    liftIO $ setForeignEncoding save
    return r
{-# INLINE withUTF8 #-}

strSize :: MonadIO m => TTFFont -> String -> m (V2 Coord)
strSize _ [] = return zero
strSize fnt str = do
    (w,h) <- withUTF8 $ TTF.sizeUTF8 fnt str
    return (V2 (fromIntegral w) (fromIntegral h))
{-# INLINE strSize #-}

renderInternal :: MonadIO m => SDL.Renderer -> SDL.Surface -> m (Maybe SDL.Texture)
renderInternal renderer sf = do
    t <- SDL.createTextureFromSurface renderer sf
    SDL.freeSurface sf
    return $ Just t
{-# INLINE renderInternal #-}

renderStr:: MonadIO m => SDL.Renderer -> TTFFont -> GuiColor -> String -> m (Maybe SDL.Texture)
renderStr _ _ _ [] = return Nothing
renderStr renderer fnt color str =
    withUTF8 $ TTF.renderUTF8Blended fnt str (toRawColor color) >>= renderInternal renderer
{-# INLINE renderStr #-}

renderStrDraft:: MonadIO m => SDL.Renderer -> TTFFont -> GuiColor -> String -> m (Maybe SDL.Texture)
renderStrDraft _ _ _ [] = return Nothing
renderStrDraft renderer fnt color str =
    withUTF8 $ TTF.renderUTF8Solid fnt str (toRawColor color) >>= renderInternal renderer
{-# INLINE renderStrDraft #-}

renderStrOpaque:: MonadIO m => SDL.Renderer -> TTFFont -> GuiColor -> GuiColor -> String -> m (Maybe SDL.Texture)
renderStrOpaque _        _   _     _       [] = return Nothing
renderStrOpaque renderer fnt color bkColor str =
    withUTF8 $ TTF.renderUTF8Shaded fnt str (toRawColor color) (toRawColor bkColor) >>= renderInternal renderer
{-# INLINE renderStrOpaque #-}

drawInternal :: MonadIO m => SDL.Renderer -> GuiPoint -> Maybe SDL.Texture -> m ()
drawInternal renderer pnt mbTexture =
    whenIsJust mbTexture $ \ t -> do
        drawTexture renderer t pnt
        SDL.destroyTexture t
{-# INLINE drawInternal #-}

drawStr :: MonadIO m => SDL.Renderer -> TTFFont -> GuiColor -> GuiPoint -> String -> m ()
drawStr renderer fnt color pnt str = renderStr renderer fnt color str >>= drawInternal renderer pnt
{-# INLINE drawStr #-}

drawStrDraft :: MonadIO m => SDL.Renderer -> TTFFont -> GuiColor -> GuiPoint -> String -> m ()
drawStrDraft renderer fnt color pnt str = renderStrDraft renderer fnt color str >>= drawInternal renderer pnt
{-# INLINE drawStrDraft #-}

drawStrOpaque :: MonadIO m => SDL.Renderer -> TTFFont -> GuiColor -> GuiColor -> GuiPoint -> String -> m ()
drawStrOpaque renderer fnt color bkColor pnt str = renderStrOpaque renderer fnt color bkColor str
                                                            >>= drawInternal renderer pnt
{-# INLINE drawStrOpaque #-}

drawStrAligned :: MonadIO m => SDL.Renderer -> TTFFont -> Alignment -> GuiColor -> DrawStrMode ->
                    GuiRect -> String -> m ()
drawStrAligned renderer fnt align color mode rect str = do
    mbTexture <- case mode of
            DrawStrFine -> renderStr renderer fnt color str
            DrawStrDraft -> renderStrDraft renderer fnt color str
            DrawStrOpaque bkColor -> renderStrOpaque renderer fnt color bkColor str
    whenIsJust mbTexture $ \ t -> do
        drawTextureAligned renderer t align rect
        SDL.destroyTexture t
