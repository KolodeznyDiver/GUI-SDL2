{-# LANGUAGE RecordWildCards #-}
module GUI.BaseLayer.Primitives(
    DrawStrMode(..),toSDLV2,fromSDLV2,toSDLRect
    ,mousePointToGuiPoint,getTextureSize,drawTexture,drawTextureAligned,fromRawColor,toRawColor,strSize,getPixelFormat
    ,createTargetTexture,renderStr,renderStrDraft,renderStrOpaque,drawStr,drawStrDraft,drawStrOpaque
    ,withStateVar,withColor,withRendererColor,withRendererTarget,withRendererClipRect,withRendererViewport
    ,drawStrAligned
                     ) where

import qualified SDL
import qualified SDL.Raw as Raw
import SDL.Vect
import Data.StateVar
import qualified SDL.TTF as TTF
import SDL.TTF.FFI (TTFFont)
import Control.Monad.IO.Class (MonadIO)
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

strSize :: MonadIO m => TTFFont -> String -> m (V2 Coord)
strSize fnt str = do
    (w,h) <- TTF.sizeUTF8 fnt str
    return (V2 (fromIntegral w) (fromIntegral h))
{-# INLINE strSize #-}

renderStr:: MonadIO m => SDL.Renderer -> TTFFont -> GuiColor -> String -> m SDL.Texture
renderStr renderer fnt color str = do
    sf <- TTF.renderUTF8Blended fnt str $ toRawColor color
    SDL.createTextureFromSurface renderer sf <* SDL.freeSurface sf

renderStrDraft:: MonadIO m => SDL.Renderer -> TTFFont -> GuiColor -> String -> m SDL.Texture
renderStrDraft renderer fnt color str = do
    sf <- TTF.renderUTF8Solid fnt str $ toRawColor color
    SDL.createTextureFromSurface renderer sf <* SDL.freeSurface sf

renderStrOpaque:: MonadIO m => SDL.Renderer -> TTFFont -> GuiColor -> GuiColor -> String -> m SDL.Texture
renderStrOpaque renderer fnt color bkColor str = do
    sf <- TTF.renderUTF8Shaded fnt str (toRawColor color) $ toRawColor bkColor
    SDL.createTextureFromSurface renderer sf <* SDL.freeSurface sf

drawStr :: MonadIO m => SDL.Renderer -> TTFFont -> GuiColor -> GuiPoint -> String -> m ()
drawStr renderer fnt color pnt str = do
    t <- renderStr renderer fnt color str
    drawTexture renderer t pnt <* SDL.destroyTexture t
{-# INLINE drawStr #-}

drawStrDraft :: MonadIO m => SDL.Renderer -> TTFFont -> GuiColor -> GuiPoint -> String -> m ()
drawStrDraft renderer fnt color pnt str = do
    t <- renderStrDraft renderer fnt color str
    drawTexture renderer t pnt <* SDL.destroyTexture t
{-# INLINE drawStrDraft #-}

drawStrOpaque :: MonadIO m => SDL.Renderer -> TTFFont -> GuiColor -> GuiColor -> GuiPoint -> String -> m ()
drawStrOpaque renderer fnt color bkColor pnt str = do
    t <- renderStrOpaque renderer fnt color bkColor str
    drawTexture renderer t pnt <* SDL.destroyTexture t
{-# INLINE drawStrOpaque #-}

drawStrAligned :: MonadIO m => SDL.Renderer -> TTFFont -> Alignment -> DrawStrMode -> GuiColor -> GuiRect -> String -> m ()
drawStrAligned renderer fnt align mode color rect str = do
    t <- case mode of
            DrawStrFine -> renderStr renderer fnt color str
            DrawStrDraft -> renderStrDraft renderer fnt color str
            DrawStrOpaque bkColor -> renderStrOpaque renderer fnt color bkColor str
    drawTextureAligned renderer t align rect <* SDL.destroyTexture t
