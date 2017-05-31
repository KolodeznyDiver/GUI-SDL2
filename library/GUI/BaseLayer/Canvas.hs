module GUI.BaseLayer.Canvas(
    -- SDL.Internal.Types
    GuiCanvas
    -- GUI.BaseLayer.Primitives
    ,DrawStrMode(..)
    -- GUI.BaseLayer.Canvas
    ,Orientation(..)
    ,toCanvasPoint,toCanvasRect,toSDLPoint,toSDLRect,runCanvas
    ,drawStretchedTexture,drawTexturePartial,drawTexture,drawTextureAligned,drawTextureEx
    ,drawLine,drawLines,drawPoint,drawPoints,drawRect,drawRects,fillRect,fillRects
    ,getTexture,getTextureFromCache
    ,drawStretchedTextureR,drawTexturePartialR,drawTextureR,drawTextureAlignedR,drawTextureExR
    ,createTargetTexture,setBlendMode,setColor,withBlendMode,withColor,withTargetTexture,withClipRect,getFont
    ,withTransparentTexture,getStrSize,
    renderStr,renderStrDraft,renderStrOpaque,drawStr,drawStrDraft,drawStrOpaque,drawStrAligned
    ,getTextSize,renderText,renderTextDraft,renderTextOpaque,drawText,drawTextDraft,drawTextOpaque,drawTextAligned
    ,drawRoundBorder,drawRoundFrame,draw3DBorder,draw3DFrame,drawDotBorder,drawArrowTriangle,drawArrow
                     ) where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import qualified SDL
import qualified SDL.Raw as Raw
import SDL.Internal.Types
import SDL.Vect
import Data.StateVar
-- import qualified SDL.TTF as TTF
import SDL.TTF.FFI (TTFFont)
--import Maybes
import Control.Monad
import Control.Monad.IO.Class -- (MonadIO)
import qualified Data.Vector.Storable as V
import qualified Data.Text as T
import GUI.BaseLayer.Types
import qualified GUI.BaseLayer.Primitives as P
import GUI.BaseLayer.Primitives (DrawStrMode(..))
import GUI.BaseLayer.Internal.Types
import GUI.BaseLayer.Resource
import GUI.BaseLayer.Geometry

data Orientation = OrientationLeft | OrientationUp | OrientationRight | OrientationDown
                 deriving (Eq, Show)

toCanvasPoint :: Canvas -> GuiPoint -> GuiPoint
toCanvasPoint c p = p .+^ canvasOffset c
{-# INLINE toCanvasPoint #-}

toCanvasRect ::  Canvas -> GuiRect -> GuiRect
toCanvasRect c (SDL.Rectangle p sz) = SDL.Rectangle (toCanvasPoint c p) sz
{-# INLINE toCanvasRect #-}

toSDLPoint :: Canvas -> GuiPoint -> SDL.Point V2 SDLCoord
toSDLPoint c p = fromIntegral <$> toCanvasPoint c p
{-# INLINE toSDLPoint #-}

toSDLRect :: Canvas -> GuiRect -> SDL.Rectangle SDLCoord
toSDLRect c = fmap fromIntegral . toCanvasRect c
{-# INLINE toSDLRect #-}

runCanvas :: MonadIO m => SDL.Renderer -> ResourceManager -> GuiCoordOffset -> GuiCanvas m a -> m a
runCanvas renderer rm off f = runReaderT f $ Canvas renderer rm off
{-# INLINE runCanvas #-}

drawStretchedTexture  :: MonadIO m => SDL.Texture -> Maybe GuiRect -> GuiRect -> GuiCanvas m ()
drawStretchedTexture t src dst = do
    c <- ask
    lift $ SDL.copy (canvasRenderer c) t (fmap P.toSDLRect src) $ Just $ toSDLRect c dst
{-# INLINE drawStretchedTexture #-}

drawTexturePartial  :: MonadIO m => SDL.Texture -> GuiRect -> GuiPoint -> GuiCanvas m ()
drawTexturePartial t src@(SDL.Rectangle _ sz) dst = do
    c <- ask
    lift $ SDL.copy (canvasRenderer c) t (Just $ P.toSDLRect src) $ Just
        $ SDL.Rectangle (toSDLPoint c dst) $ P.toSDLV2 sz
{-# INLINE drawTexturePartial #-}

drawTexture :: MonadIO m => SDL.Texture -> GuiPoint -> GuiCanvas m ()
drawTexture texture pnt =  do{ c <- ask; lift $ P.drawTexture (canvasRenderer c) texture $ toCanvasPoint c pnt}
{-# INLINE drawTexture #-}

drawTextureAligned :: MonadIO m => SDL.Texture -> Alignment -> GuiRect -> GuiCanvas m ()
drawTextureAligned texture align rect =  do
    c <- ask
    lift $ P.drawTextureAligned (canvasRenderer c) texture align $ toCanvasRect c rect
{-# INLINE drawTextureAligned #-}

drawTextureEx :: MonadIO m => SDL.Texture -> Maybe GuiRect -> GuiRect -> Double -> Maybe GuiPoint
                    -> Bool -> Bool -> GuiCanvas m ()
drawTextureEx texture src dst rotateAngle rotPnt flipV flipH =  do
    c <- ask
    lift $ SDL.copyEx (canvasRenderer c) texture (fmap P.toSDLRect src)
        (Just $ toSDLRect c dst)  (realToFrac rotateAngle) (fmap (toSDLPoint c) rotPnt) $ V2 flipV flipH
{-# INLINE drawTextureEx #-}

drawLine :: MonadIO m => GuiPoint -> GuiPoint -> GuiCanvas m ()
drawLine p0 p1 =  do { c <- ask; lift $ SDL.drawLine (canvasRenderer c) (toSDLPoint c p0) (toSDLPoint c p1)}
{-# INLINE drawLine #-}

drawLines :: MonadIO m => V.Vector GuiPoint -> GuiCanvas m ()
drawLines v = do{ c <- ask; lift $ SDL.drawLines (canvasRenderer c) $ V.map (toSDLPoint c) v}

drawPoint :: MonadIO m => GuiPoint -> GuiCanvas m ()
drawPoint p = do{ c <- ask; lift $ SDL.drawPoint (canvasRenderer c) $ toSDLPoint c p}
{-# INLINE drawPoint #-}

drawPoints :: MonadIO m => V.Vector GuiPoint -> GuiCanvas m ()
drawPoints v = do{ c <- ask; lift $ SDL.drawPoints (canvasRenderer c) $ V.map (toSDLPoint c) v}
{-# INLINE drawPoints #-}

drawRect :: MonadIO m => GuiRect -> GuiCanvas m ()
drawRect r = do{ c <- ask; lift $ SDL.drawRect (canvasRenderer c) $ Just $ toSDLRect c r}
{-# INLINE drawRect #-}

drawRects :: MonadIO m => V.Vector GuiRect -> GuiCanvas m ()
drawRects v = do{ c <- ask; lift $ SDL.drawRects (canvasRenderer c) $ V.map (toSDLRect c) v}
{-# INLINE drawRects #-}

fillRect :: MonadIO m => GuiRect -> GuiCanvas m ()
fillRect r = do{ c <- ask; lift $ SDL.fillRect (canvasRenderer c) $ Just $ toSDLRect c r}
{-# INLINE fillRect #-}

fillRects :: MonadIO m => V.Vector GuiRect -> GuiCanvas m ()
fillRects v = do{ c <- ask; lift $ SDL.fillRects (canvasRenderer c) $ V.map (toSDLRect c) v}
{-# INLINE fillRects #-}

getTextureFromCache :: MonadIO m => T.Text -> GuiCanvas m (Maybe SDL.Texture)
getTextureFromCache k = do{ rm <- asks canvasRM; lift $ rmGetTextureFromCache rm k}
{-# INLINE getTextureFromCache #-}

getTexture :: MonadIO m => T.Text -> GuiCanvas m SDL.Texture
getTexture k = do{ c <- ask; lift $ rmGetTexture (canvasRM c) (canvasRenderer c) k}
{-# INLINE getTexture #-}

drawStretchedTextureR  :: MonadIO m => T.Text -> Maybe GuiRect -> GuiRect -> GuiCanvas m ()
drawStretchedTextureR k src dst = do { t <- getTexture k; drawStretchedTexture t src dst}
{-# INLINE drawStretchedTextureR #-}

drawTexturePartialR  :: MonadIO m => T.Text -> GuiRect -> GuiPoint -> GuiCanvas m ()
drawTexturePartialR k src p = do { t <- getTexture k; drawTexturePartial t src p}
{-# INLINE drawTexturePartialR #-}

drawTextureR :: MonadIO m => T.Text -> GuiPoint -> GuiCanvas m ()
drawTextureR k p = do { t <- getTexture k; drawTexture t p}
{-# INLINE drawTextureR #-}

drawTextureAlignedR :: MonadIO m => T.Text -> Alignment -> GuiRect -> GuiCanvas m ()
drawTextureAlignedR k align rect = do { t <- getTexture k; drawTextureAligned t align rect}
{-# INLINE drawTextureAlignedR #-}

drawTextureExR :: MonadIO m => T.Text -> Maybe GuiRect -> GuiRect -> Double -> Maybe GuiPoint
                    -> Bool -> Bool -> GuiCanvas m ()
drawTextureExR k src dst rotateAngle rotPnt flipV flipH =
    do { t <- getTexture k; drawTextureEx t src dst rotateAngle rotPnt flipV flipH}
{-# INLINE drawTextureExR #-}

createTargetTexture :: MonadIO m => GuiSize -> GuiCanvas m SDL.Texture
createTargetTexture sz = do { renderer <- asks canvasRenderer; lift $ P.createTargetTexture renderer sz}
{-# INLINE createTargetTexture #-}

setBlendMode :: MonadIO m => SDL.BlendMode -> GuiCanvas m ()
setBlendMode m = do { renderer <- asks canvasRenderer; lift (SDL.rendererDrawBlendMode renderer $= m)}
{-# INLINE setBlendMode #-}

setColor  :: MonadIO m => GuiColor -> GuiCanvas m ()
setColor (V4 r g b a) = do
    (SDL.Internal.Types.Renderer re) <- asks canvasRenderer
    void $ lift $ Raw.setRenderDrawColor re r g b a
{-# INLINE setColor #-}

withStateVar :: MonadIO m => StateVar a -> a -> GuiCanvas m b -> GuiCanvas m b
withStateVar sv a b = do {save <- lift $ get sv; lift (sv $= a); r <- b; lift (sv $= save); return r}
{-# INLINE withStateVar #-}

withBlendMode :: MonadIO m => SDL.BlendMode -> GuiCanvas m a -> GuiCanvas m a
withBlendMode m a = do { renderer <- asks canvasRenderer; withStateVar (SDL.rendererDrawBlendMode renderer) m a}
{-# INLINE withBlendMode #-}

withColor :: MonadIO m => GuiColor -> GuiCanvas m a -> GuiCanvas m a
withColor color a = do { renderer <- asks canvasRenderer; withStateVar (SDL.rendererDrawColor renderer) color a}
{-# INLINE withColor #-}

withTargetTexture :: MonadIO m => SDL.Texture -> GuiCanvas m a -> GuiCanvas m a
withTargetTexture t a = do
    renderer <- asks canvasRenderer
    -- SDL bug: ClipRect=Just (Rectangle (P (V2 0 0)) (V2 0 0)) after set RenderTarget
    let clipRect = SDL.rendererClipRect renderer
        blendMode = SDL.rendererDrawBlendMode renderer
        logicalSize = SDL.rendererLogicalSize renderer
        scale = SDL.rendererScale renderer
        viewport = SDL.rendererViewport renderer
    svClipRect <- lift $ get clipRect
    svBlendMode <- lift $ get blendMode
    svLogicalSize <- lift $ get logicalSize
    svScale <- lift $ get scale
    svViewport <- lift $ get viewport
{-    liftIO $ putStrLn $ concat ["withTargetTexture save : svClipRect", show svClipRect,
        "  svBlendMode=", show svBlendMode, "  svLogicalSize=", show svLogicalSize,
        "  svScale=", show svScale, "  svViewport=", show svViewport] -}
    withStateVar (SDL.rendererRenderTarget renderer) (Just t) (do
--            tst <- lift $ get clipRect
--            liftIO $ putStrLn $ concat ["withTargetTexture ",show tst]
            textureSz <- liftIO $ P.getTextureSize t
            let r = P.toSDLRect $ SDL.Rectangle zero textureSz
            lift $ --do
--                scale $= V2 1.0 1.0
--                logicalSize $= Just (V2 1 1)
--                viewport $= Just r
                clipRect $= Just r
--                blendMode $= SDL.BlendNone
            a)
        <* lift (do
                viewport $= svViewport
                scale $= svScale
                logicalSize $= svLogicalSize
                blendMode $= svBlendMode
                clipRect $= svClipRect
{-                liftIO $ putStrLn $ concat ["withTargetTexture restore : svClipRect", show svClipRect,
                        "  svBlendMode=", show svBlendMode, "  svLogicalSize=", show svLogicalSize,
                        "  svScale=", show svScale, "  svViewport=", show svViewport] -}
                    )
{-# INLINE withTargetTexture #-}

withClipRect :: MonadIO m => GuiRect -> GuiCanvas m a -> GuiCanvas m a
withClipRect rect f = do
    c@Canvas{canvasRenderer=renderer} <- ask
    let clipRect = SDL.rendererClipRect renderer
    sv <- lift $ get clipRect
    let newClipRect = toSDLRect c rect
        resultClipRect = case sv of
            Just oldClipRect | isEmptyRect oldClipRect -> newClipRect
                             | otherwise -> rectIntersection newClipRect oldClipRect
            _ -> newClipRect
{-    liftIO $ putStrLn $ concat ["withClipRect sv=",show sv, " newClipRect=", show newClipRect,
        "  resultClipRect=", show resultClipRect] -}
    lift $ clipRect $= Just resultClipRect
    r <- f
    lift $ clipRect $= sv
    return r

withTransparentTexture :: MonadIO m => GuiTransparency -> SDL.Texture -> GuiCanvas m a -> GuiCanvas m a
withTransparentTexture transparency texture f = do
    let tam = SDL.textureAlphaMod   texture
        tbm = SDL.textureBlendMode  texture
    (svAM,svBM) <- lift $ do
        a <- get tam
        b <- get tbm
        tam $= transparency
        tbm $= SDL.BlendAlphaBlend
        return (a,b)
    r <- f
    lift $ do
        tam $= svAM
        tbm $= svBM
    return r

getFont:: MonadIO m => T.Text -> GuiCanvas m TTFFont
getFont k = do { rm <- asks canvasRM; lift $ rmGetFont rm k}
{-# INLINE getFont #-}
------------------------------------------------------------------------------------------------------
getStrSize :: MonadIO m => TTFFont -> String -> GuiCanvas m (V2 Coord)
getStrSize fnt = lift . P.strSize fnt
{-# INLINE getStrSize #-}

renderStr:: MonadIO m => TTFFont -> GuiColor -> String -> GuiCanvas m (Maybe SDL.Texture)
renderStr fnt color str = do { renderer <- asks canvasRenderer; lift $ P.renderStr renderer fnt color str}
{-# INLINE renderStr #-}

renderStrDraft:: MonadIO m => TTFFont -> GuiColor -> String -> GuiCanvas m (Maybe SDL.Texture)
renderStrDraft fnt color str = do { renderer <- asks canvasRenderer; lift $ P.renderStrDraft renderer fnt color str}
{-# INLINE renderStrDraft #-}

renderStrOpaque:: MonadIO m => TTFFont -> GuiColor -> GuiColor -> String -> GuiCanvas m (Maybe SDL.Texture)
renderStrOpaque fnt color bkColor str = do
    renderer <- asks canvasRenderer
    lift $ P.renderStrOpaque renderer fnt color bkColor str
{-# INLINE renderStrOpaque #-}

drawStr :: MonadIO m => TTFFont -> GuiColor -> GuiPoint -> String -> GuiCanvas m ()
drawStr fnt color pnt str = do{ c <- ask; lift $ P.drawStr (canvasRenderer c) fnt color (toCanvasPoint c pnt) str}
{-# INLINE drawStr #-}

drawStrDraft :: MonadIO m => TTFFont -> GuiColor -> GuiPoint -> String -> GuiCanvas m ()
drawStrDraft fnt color pnt str = do
    c <- ask
    lift $ P.drawStrDraft (canvasRenderer c) fnt color (toCanvasPoint c pnt) str
{-# INLINE drawStrDraft #-}

drawStrOpaque :: MonadIO m => TTFFont -> GuiColor -> GuiColor -> GuiPoint -> String -> GuiCanvas m ()
drawStrOpaque fnt color bkColor pnt str = do
    c <- ask
    lift $ P.drawStrOpaque (canvasRenderer c) fnt color bkColor (toCanvasPoint c pnt) str
{-# INLINE drawStrOpaque #-}

drawStrAligned :: MonadIO m => TTFFont -> Alignment -> GuiColor -> DrawStrMode -> GuiRect -> String -> GuiCanvas m ()
drawStrAligned fnt align color mode rect str = do
    c <- ask
    lift $  P.drawStrAligned (canvasRenderer c) fnt align color mode (toCanvasRect c rect) str
{-# INLINE drawStrAligned #-}
------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------
getTextSize :: MonadIO m => TTFFont -> T.Text -> GuiCanvas m (V2 Coord)
getTextSize fnt = getStrSize fnt . T.unpack
{-# INLINE getTextSize #-}

renderText:: MonadIO m => TTFFont -> GuiColor -> T.Text -> GuiCanvas m (Maybe SDL.Texture)
renderText fnt color = renderStr fnt color . T.unpack
{-# INLINE renderText #-}

renderTextDraft:: MonadIO m => TTFFont -> GuiColor -> T.Text -> GuiCanvas m (Maybe SDL.Texture)
renderTextDraft fnt color = renderStrDraft fnt color . T.unpack
{-# INLINE renderTextDraft #-}

renderTextOpaque:: MonadIO m => TTFFont -> GuiColor -> GuiColor -> T.Text -> GuiCanvas m (Maybe SDL.Texture)
renderTextOpaque fnt color bkColor = renderStrOpaque fnt color bkColor . T.unpack
{-# INLINE renderTextOpaque #-}

drawText :: MonadIO m => TTFFont -> GuiColor -> GuiPoint -> T.Text -> GuiCanvas m ()
drawText fnt color pnt = drawStr fnt color pnt . T.unpack
{-# INLINE drawText #-}

drawTextDraft :: MonadIO m => TTFFont -> GuiColor -> GuiPoint -> T.Text -> GuiCanvas m ()
drawTextDraft fnt color pnt = drawStrDraft fnt color pnt . T.unpack
{-# INLINE drawTextDraft #-}

drawTextOpaque :: MonadIO m => TTFFont -> GuiColor -> GuiColor -> GuiPoint -> T.Text -> GuiCanvas m ()
drawTextOpaque fnt color bkColor pnt = drawStrOpaque fnt color bkColor pnt . T.unpack
{-# INLINE drawTextOpaque #-}

drawTextAligned :: MonadIO m => TTFFont -> Alignment -> GuiColor -> DrawStrMode -> GuiRect -> T.Text -> GuiCanvas m ()
drawTextAligned fnt align color mode rect = drawStrAligned fnt align color mode rect . T.unpack
{-# INLINE drawTextAligned #-}
------------------------------------------------------------------------------------------------------

drawRoundBorder :: MonadIO m => GuiRect -> GuiCanvas m ()
drawRoundBorder (SDL.Rectangle p (V2 w' h')) =
    let radius = 2
        w = w' - 1
        h = h' - 1 in
    drawLines $ V.fromList [
        p .+^ V2 0 radius, p .+^ V2 radius 0,
        p .+^ V2 (w-radius) 0, p .+^ V2 w radius,
        p .+^ V2 w (h-radius), p .+^ V2 (w-radius) h,
        p .+^ V2 radius h, p .+^ V2 0 (h-radius),    p .+^ V2 0 radius  ]

drawRoundFrame :: MonadIO m => GuiColor -> GuiColor -> GuiColor -> GuiRect -> GuiCanvas m ()
drawRoundFrame outsideColor borderColor insideColor r@(SDL.Rectangle p (V2 w' h')) = do
    setColor insideColor
    fillRect r
    setColor outsideColor
    let w = w' - 1
        h = h' - 1
    drawPoints $ V.concat [cornPnts 1 1 p, cornPnts  (-1) 1 $ p .+^ V2 w 0,
        cornPnts (-1) (-1) $ p .+^ V2 w h, cornPnts 1 (-1) $ p .+^ V2 0 h]
    setColor borderColor
    drawRoundBorder r
  where cornPnts :: Coord -> Coord -> GuiPoint -> V.Vector GuiPoint
        cornPnts dx dy pt = V.fromList [pt, pt .+^ V2 dx 0, pt .+^ V2 0 dy]

draw3DBorder :: MonadIO m => GuiColor -> GuiColor -> Int -> GuiRect -> GuiCanvas m ()
draw3DBorder lightColor darkColor thickness (SDL.Rectangle p (V2 w' h')) = do
    let w = w' - 1
        h = h' - 1
    setColor darkColor
    cornPnts (-w) (-h) (p .+^ V2 w h)
    setColor lightColor
    cornPnts w h p
  where cornPnts dx' dy' pt' =
            let sg = signum dx'
                go dx dy pt cnt = do
                    drawLines $ V.fromList [pt .+^ V2 dx 0 , pt , pt .+^ V2 0 dy]
                    when (cnt>1) $ go (dx - 2*sg) (dy - 2*sg) (pt .+^ V2 sg sg) (cnt-1)
            in go dx' dy' pt' thickness

draw3DFrame :: MonadIO m => GuiColor -> GuiColor -> GuiColor -> Int -> GuiRect -> GuiCanvas m ()
draw3DFrame lightColor darkColor insideColor thickness r = do
    setColor insideColor
    fillRect r
    draw3DBorder lightColor darkColor thickness r

drawDotBorder :: MonadIO m => Coord -> GuiRect -> GuiCanvas m ()
drawDotBorder step = drawPoints . mkDotRectVector step
{-# INLINE drawDotBorder #-}

drawArrowTriangle :: MonadIO m => Orientation -> GuiColor -> GuiPoint -> Coord -> GuiCanvas m ()
drawArrowTriangle orient color {-center@-}(P(V2 x y)) h = do
    let sqrt2 = sqrt 0.5 :: Double
        quadrateSide = truncate $ sqrt2 * fromIntegral h
        szQ = V2 quadrateSide quadrateSide
        qHlfSd = quadrateSide `div` 2
        szHlf = V2 qHlfSd qHlfSd
        h2 = h `div` 2
        h4 = h `div` 4
    t <- createTargetTexture szQ
    withTargetTexture t $ do
        setColor color
        c <- ask
        lift $ SDL.fillRect (canvasRenderer c) Nothing
    let (cP,clipR) =
            case orient of
                OrientationLeft   -> (P(V2 (x + h4) y),SDL.Rectangle (P(V2 (x-h4) (y-h2))) (V2 h2 h))
                OrientationUp    -> (P(V2 x (y + h4)),SDL.Rectangle (P(V2 (x-h2) (y-h4))) (V2 h h2))
                OrientationRight  -> (P(V2 (x - h4) y),SDL.Rectangle (P(V2 (x-h4) (y-h2))) (V2 h2 h))
                OrientationDown -> (P(V2 x (y - h4)),SDL.Rectangle (P(V2 (x-h2) (y-h4))) (V2 h h2))
        dstR = SDL.Rectangle (cP .-^ szHlf) szQ
{-    setColor $ V4 255 0 0 0
    drawRect dstR
    setColor $ V4 255 255 0 0
    drawRect clipR -}
    withClipRect clipR $
        drawTextureEx t Nothing dstR 45.0 Nothing False False
    lift $ SDL.destroyTexture t

drawArrow :: MonadIO m => Orientation -> GuiPoint -> Coord -> GuiCanvas m ()
drawArrow orient center h =
    let go dlt =
            let p = center .-^ fmap (`div` 2) dlt
                go' 0 _ = return ()
                go' i inc@(V2 dx dy) = let incRev = V2 dy dx in
                                       drawLines (V.fromList [ p .-^ incRev, p .+^ inc, p .+^ incRev]) >>
                                          go' (i-1) (inc - fmap signum inc)
            in go' (h-1) dlt
    in go (case orient of
                OrientationLeft   -> V2 (-h) 0
                OrientationUp    ->  V2 0 (-h)
                OrientationRight  -> V2 h 0
                OrientationDown -> V2 0 h)


