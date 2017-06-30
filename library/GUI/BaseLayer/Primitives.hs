{-# LANGUAGE RecordWildCards #-}
-- |
-- Module:      GUI.BaseLayer.Primitives
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <kolodeznydiver@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Вспомогательные функции, относящиеся к графике, адаптирующие SDL к GUI.

module GUI.BaseLayer.Primitives(
    -- * Преобразование типов.
    toSDLV2,fromSDLV2,toSDLRect,mousePointToGuiPoint
    -- * Функции, вызываемые, восновном, из "GUI.BaseLayer.Canvas".
    ,getTextureSize,drawTexture,drawTextureAligned,fromRawColor,toRawColor,strSize,getPixelFormat
    ,createTargetTexture,renderStr,renderStrDraft,renderStrOpaque,drawStr,drawStrDraft,drawStrOpaque
    ,withStateVar,withColor,withRendererColor,withRendererTarget,withRendererClipRect,withRendererViewport
    ,DrawStrMode(..),drawStrAligned
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
import GUI.BaseLayer.Depend0.Types
import GUI.BaseLayer.Depend0.Auxiliaries
import GUI.BaseLayer.Depend1.Geometry

-- | Преобразование из @V2 Coord@ в @V2 SDLCoord@.
toSDLV2 :: V2 Coord -> V2 SDLCoord
toSDLV2 = fmap fromIntegral
{-# INLINE toSDLV2 #-}

-- | Преобразование из @V2 SDLCoord@ в @V2 Coord@.
fromSDLV2 :: V2 SDLCoord -> V2 Coord
fromSDLV2 = fmap fromIntegral
{-# INLINE fromSDLV2 #-}

-- | Преобразование из 'GuiRect' в @SDL.Rectangle SDLCoord@.
toSDLRect :: GuiRect -> SDL.Rectangle SDLCoord
toSDLRect (SDL.Rectangle (SDL.P p) sz) = SDL.Rectangle (P $ toSDLV2 p) (toSDLV2 sz)
{-# INLINE toSDLRect #-}

-- | Преобразование из 'MousePoint' в 'GuiPoint'.
mousePointToGuiPoint:: MousePoint -> GuiPoint
mousePointToGuiPoint = fmap fromIntegral
{-# INLINE mousePointToGuiPoint #-}

-- | Преобразование из 'Raw.Color' в 'GuiColor'.
fromRawColor :: Raw.Color -> GuiColor
fromRawColor Raw.Color{..} = V4 colorR colorG colorB colorA
{-# INLINE fromRawColor #-}

-- | Преобразование из 'GuiColor' в 'Raw.Color'.
toRawColor :: GuiColor -> Raw.Color
toRawColor (V4 r g b a) = Raw.Color r g b a
{-# INLINE toRawColor #-}

-- | Возвращает размеры текстуры.
getTextureSize :: MonadIO m => SDL.Texture -> m GuiSize
getTextureSize = fmap (\ti -> fromIntegral <$> V2 (SDL.textureWidth ti) (SDL.textureHeight ti)) . SDL.queryTexture
{-# INLINE getTextureSize #-}

-- | Рисует текстуру на указанном 'SDL.Renderer', в указанной точке (левый верхний угол) не изменяя её размер.
drawTexture :: MonadIO m => SDL.Renderer -> SDL.Texture -> GuiPoint -> m ()
drawTexture renderer texture pnt = do
    sz <- getTextureSize texture
    SDL.copy renderer texture Nothing $ Just $ toSDLRect $ SDL.Rectangle pnt sz

-- | Рисует текстуру на указанном 'SDL.Renderer', в заданном прямоугольнике с заданным выравниванием,
-- не изменяя её размер.
drawTextureAligned :: MonadIO m => SDL.Renderer -> SDL.Texture -> Alignment -> GuiRect -> m ()
drawTextureAligned renderer texture align rect = do
    sz <- getTextureSize texture
    SDL.copy renderer texture Nothing $ Just $ toSDLRect $ rectAlign align sz rect

-- | Возвращает 'SDL.PixelFormat'
getPixelFormat :: MonadIO m => SDL.Renderer -> m SDL.PixelFormat
getPixelFormat = fmap (head . SDL.rendererInfoTextureFormats) . SDL.getRendererInfo
{-# INLINE getPixelFormat #-}

-- | Создаёт текстуру совместимую с указанным 'SDL.Renderer' заданного размера которая может
-- быть буфером для создания изображения.
createTargetTexture :: MonadIO m => SDL.Renderer -> GuiSize -> m SDL.Texture
createTargetTexture renderer sz = do    pf <- getPixelFormat renderer
                                        SDL.createTexture renderer pf SDL.TextureAccessTarget $ toSDLV2 sz
{-# INLINE createTargetTexture #-}

-- | Вспомогательная функция сохраняющая значение типа __/a/__ с помощью @StateVar a@,
-- устанавливающая заданное значение, и, после выполнения переданной ей функции
-- восстанавливающая сохранённое значение.
withStateVar ::  MonadIO m => StateVar a -> a -> m b -> m b
withStateVar sv a b = do {save <- get sv; sv $= a; r <- b; sv $= save; return r}
{-# INLINE withStateVar #-}

-- | Для переданной третьим аргументом функции временно устанавливает заданный цвет рисования.
withColor :: MonadIO m => GuiCurDrawColor -> GuiColor -> m a -> m a
withColor = withStateVar
{-# INLINE withColor #-}

-- | Для переданной третьим аргументом функции временно устанавливает заданный цвет рисования
-- для заданного 'SDL.Renderer'.
withRendererColor :: MonadIO m => SDL.Renderer -> GuiColor -> m a -> m a
withRendererColor renderer = withStateVar (SDL.rendererDrawColor renderer)
{-# INLINE withRendererColor #-}

-- | Для переданной третьим аргументом функции временно устанавливает для заданного 'SDL.Renderer'
-- заданную текстуру для рисования в ней.
withRendererTarget :: MonadIO m => SDL.Renderer -> SDL.Texture -> m a -> m a
withRendererTarget renderer t = withStateVar (SDL.rendererRenderTarget renderer) $ Just t
{-# INLINE withRendererTarget #-}

-- | Для переданной третьим аргументом функции временно устанавливает заданный прямоугольник как
-- ограничитель области рисования для заданного 'SDL.Renderer'.
withRendererClipRect :: MonadIO m => SDL.Renderer -> GuiRect -> m a -> m a
withRendererClipRect renderer r = withStateVar (SDL.rendererClipRect renderer) $ Just $ toSDLRect r
{-# INLINE withRendererClipRect #-}

-- | Для переданной третьим аргументом функции временно устанавливает заданный прямоугольник как
-- Viewport для заданного 'SDL.Renderer'.
withRendererViewport :: MonadIO m => SDL.Renderer -> GuiRect -> m a -> m a
withRendererViewport renderer r = withStateVar (SDL.rendererViewport renderer) $ Just $ toSDLRect r
{-# INLINE withRendererViewport #-}

-- | Возвращает размер который будет занимать на экране заданная строка выведенная заданным шрифтом.
strSize :: MonadIO m => TTFFont -> String -> m (V2 Coord)
strSize _ [] = return zero
strSize fnt str = do
    (w,h) <- withUTF8 $ TTF.sizeUTF8 fnt str
    return (V2 (fromIntegral w) (fromIntegral h))
{-# INLINE strSize #-}

-- No exported.
renderInternal :: MonadIO m => SDL.Renderer -> SDL.Surface -> m (Maybe SDL.Texture)
renderInternal renderer sf = do
    t <- SDL.createTextureFromSurface renderer sf
    SDL.freeSurface sf
    return $ Just t
{-# INLINE renderInternal #-}

-- | Создание текстуры из строки с заданным шрифтом и цветом.
-- Качественно, с полутонами, но медленно.
renderStr:: MonadIO m => SDL.Renderer -> TTFFont -> GuiColor -> String -> m (Maybe SDL.Texture)
renderStr _ _ _ [] = return Nothing
renderStr renderer fnt color str =
    withUTF8 $ TTF.renderUTF8Blended fnt str (toRawColor color) >>= renderInternal renderer
{-# INLINE renderStr #-}

-- | Создание текстуры из строки с заданным шрифтом и цветом.
-- Без полутонов, но быстро.
renderStrDraft:: MonadIO m => SDL.Renderer -> TTFFont -> GuiColor -> String -> m (Maybe SDL.Texture)
renderStrDraft _ _ _ [] = return Nothing
renderStrDraft renderer fnt color str =
    withUTF8 $ TTF.renderUTF8Solid fnt str (toRawColor color) >>= renderInternal renderer
{-# INLINE renderStrDraft #-}

-- | Создание текстуры из строки с заданным шрифтом и цветом.
-- С полутонами и быстро за счёт непрозрачного рисования по указанному фоновому цвету.
renderStrOpaque:: MonadIO m => SDL.Renderer -> -- ^ 'SDL.Renderer'.
                               TTFFont -> -- ^ Шрифт.
                               GuiColor -> -- ^ Цвет текста.
                               GuiColor -> -- ^ Цвет фона.
                               String -> -- ^ Выводимая строка.
                               m (Maybe SDL.Texture)
renderStrOpaque _        _   _     _       [] = return Nothing
renderStrOpaque renderer fnt color bkColor str =
    withUTF8 $ TTF.renderUTF8Shaded fnt str (toRawColor color) (toRawColor bkColor) >>= renderInternal renderer
{-# INLINE renderStrOpaque #-}

-- No exported.
drawInternal :: MonadIO m => SDL.Renderer -> GuiPoint -> Maybe SDL.Texture -> m ()
drawInternal renderer pnt mbTexture =
    whenIsJust mbTexture $ \ t -> do
        drawTexture renderer t pnt
        SDL.destroyTexture t
{-# INLINE drawInternal #-}

-- | Отрисовка строки с заданным шрифтом и цветом на указанном 'SDL.Renderer' в заданной точке.
-- Качественно, с полутонами, но медленно.
drawStr :: MonadIO m => SDL.Renderer -> TTFFont -> GuiColor -> GuiPoint -> String -> m ()
drawStr renderer fnt color pnt str = renderStr renderer fnt color str >>= drawInternal renderer pnt
{-# INLINE drawStr #-}

-- | Отрисовка строки с заданным шрифтом и цветом на указанном 'SDL.Renderer' в заданной точке.
-- Без полутонов, но быстро.
drawStrDraft :: MonadIO m => SDL.Renderer -> TTFFont -> GuiColor -> GuiPoint -> String -> m ()
drawStrDraft renderer fnt color pnt str = renderStrDraft renderer fnt color str >>= drawInternal renderer pnt
{-# INLINE drawStrDraft #-}

-- | Отрисовка строки с заданным шрифтом и цветом на указанном 'SDL.Renderer' в заданной точке.
-- С полутонами и быстро за счёт непрозрачного рисования по указанному фоновому цвету.
drawStrOpaque :: MonadIO m => SDL.Renderer -> -- ^ 'SDL.Renderer'.
                              TTFFont -> -- ^ Шрифт.
                              GuiColor -> -- ^ Цвет текста.
                              GuiColor -> -- ^ Цвет фона.
                              GuiPoint -> -- ^ Позиция вывода.
                              String -> -- ^ Выводимая строка.
                              m ()
drawStrOpaque renderer fnt color bkColor pnt str = renderStrOpaque renderer fnt color bkColor str
                                                            >>= drawInternal renderer pnt
{-# INLINE drawStrOpaque #-}

-- | Режим отрисовки текста для универсальных функций вывода текста.
data DrawStrMode = DrawStrFine             -- ^ По прозрачному фону, качественно и медленно.
                 | DrawStrDraft            -- ^ По прозрачному фону, без полутонов но быстро.
                 | DrawStrOpaque GuiColor  -- ^ По заданному непрозрачному фону.
                 deriving (Eq, Show)

-- | Отрисовка строки с заданным шрифтом и цветом на указанном 'SDL.Renderer',
--  с заданным выравниванием в пределах заданного прямоугольника и с заданным режимом отрисовки.
drawStrAligned :: MonadIO m => SDL.Renderer -> -- ^ 'SDL.Renderer'.
                               TTFFont -> -- ^ Шрифт.
                               Alignment -> -- ^ Режим выравнивания.
                               GuiColor -> -- ^ Цвет текста.
                               DrawStrMode -> -- ^ Режим отрисовки.
                               GuiRect -> -- ^ Прясоугольная область для вывода с учётом выравнивания.
                               String -> -- ^ Выводимая строка.
                               m ()
drawStrAligned renderer fnt align color mode rect str = do
    mbTexture <- case mode of
            DrawStrFine -> renderStr renderer fnt color str
            DrawStrDraft -> renderStrDraft renderer fnt color str
            DrawStrOpaque bkColor -> renderStrOpaque renderer fnt color bkColor str
    whenIsJust mbTexture $ \ t -> do
        drawTextureAligned renderer t align rect
        SDL.destroyTexture t
