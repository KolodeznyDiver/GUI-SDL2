{-# LANGUAGE RecordWildCards #-}
-- |
-- Module:      GUI.BaseLayer.Primitives
-- Copyright:   (c) 2017-2018 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Вспомогательные функции, относящиеся к графике, адаптирующие SDL к GUI.

module GUI.BaseLayer.Primitives(
    -- * Преобразование типов.
    toSDLV2,fromSDLV2,toSDLPoint,fromSDLPoint,toSDLRect,mousePointToGuiPoint
    -- * Функции, вызываемые, восновном, из "GUI.BaseLayer.Canvas".
    ,getTextureSize,drawTexture,drawTextureAligned,fromRawColor,textSize,getPixelFormat
    ,createTargetTexture,renderText,renderTextDraft,renderTextOpaque,renderCharOpaque
    ,drawText,drawTextDraft,drawTextOpaque,drawCharOpaque
    ,withStateVar,withColor,withRendererColor,withRendererTarget,withRendererClipRect,withRendererViewport
    ,DrawStrMode(..),drawStrAligned
    -- * Прочие
    ,getDisplayRectByPoint
                     ) where

import Data.Maybe
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T
import Data.StateVar
import Control.Monad.Extra (whenJust)
import qualified SDL
import qualified SDL.Raw as Raw
import SDL.Vect
import qualified SDL.Font as FNT
import SDL.Font
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

-- | Преобразование из 'GuiPoint' в @SDL.Point V2 SDLCoord@.
toSDLPoint :: GuiPoint -> SDL.Point V2 SDLCoord
toSDLPoint (P p) = P (toSDLV2 p)
{-# INLINE toSDLPoint #-}

-- | Преобразование из @SDL.Point V2 SDLCoord@ в 'GuiPoint'.
fromSDLPoint :: SDL.Point V2 SDLCoord -> GuiPoint
fromSDLPoint (P p) = P (fromSDLV2 p)
{-# INLINE fromSDLPoint #-}

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
{-
-- | Преобразование из 'GuiColor' в 'Raw.Color'.
toRawColor :: GuiColor -> Raw.Color
toRawColor (V4 r g b a) = Raw.Color r g b a
{-# INLINE toRawColor #-}
-}
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
textSize :: MonadIO m => Font -> T.Text -> m (V2 Coord)
textSize fnt txt
    | T.null txt = return zero -- FNT.lineSkip
    | otherwise  = do (w,h) <- withUTF8 $ FNT.size fnt txt
                      return (V2 (fromIntegral w) (fromIntegral h))
{-# INLINE textSize #-}

-- No exported.
renderInternal' :: MonadIO m => SDL.Renderer -> SDL.Surface -> m SDL.Texture
renderInternal' renderer sf = do
    t <- SDL.createTextureFromSurface renderer sf
    SDL.freeSurface sf
    return t
{-# INLINE renderInternal' #-}

-- No exported.
renderInternal :: MonadIO m => SDL.Renderer -> SDL.Surface -> m (Maybe SDL.Texture)
renderInternal renderer = fmap Just . renderInternal' renderer
{-# INLINE renderInternal #-}

-- | Создание текстуры из строки с заданным шрифтом и цветом.
-- Качественно, с полутонами, но медленно.
renderText:: MonadIO m => SDL.Renderer -> Font -> GuiColor -> T.Text -> m (Maybe SDL.Texture)
renderText renderer fnt color txt
    | T.null txt = return Nothing
    | otherwise  = withUTF8 $ FNT.blended fnt color txt >>= renderInternal renderer
{-# INLINE renderText #-}

-- | Создание текстуры из строки с заданным шрифтом и цветом.
-- Без полутонов, но быстро.
renderTextDraft:: MonadIO m => SDL.Renderer -> Font -> GuiColor -> T.Text -> m (Maybe SDL.Texture)
renderTextDraft renderer fnt color txt
    | T.null txt = return Nothing
    | otherwise  = withUTF8 $ FNT.solid fnt color txt >>= renderInternal renderer
{-# INLINE renderTextDraft #-}

-- | Создание текстуры из строки с заданным шрифтом и цветом.
-- С полутонами и быстро за счёт непрозрачного рисования по указанному фоновому цвету.
renderTextOpaque:: MonadIO m => SDL.Renderer -> -- ^ 'SDL.Renderer'.
                               Font -> -- ^ Шрифт.
                               GuiColor -> -- ^ Цвет текста.
                               GuiColor -> -- ^ Цвет фона.
                               T.Text -> -- ^ Выводимая строка.
                               m (Maybe SDL.Texture)
renderTextOpaque renderer fnt color bkColor txt
    | T.null txt = return Nothing
    | otherwise  = withUTF8 $ FNT.shaded fnt color bkColor txt >>= renderInternal renderer
{-# INLINE renderTextOpaque #-}

-- | Создание текстуры из одного символа с заданным шрифтом и цветом.
-- С полутонами и быстро за счёт непрозрачного рисования по указанному фоновому цвету.
renderCharOpaque:: MonadIO m => SDL.Renderer -> -- ^ 'SDL.Renderer'.
                               Font -> -- ^ Шрифт.
                               GuiColor -> -- ^ Цвет текста.
                               GuiColor -> -- ^ Цвет фона.
                               Char -> -- ^ Выводимый символ.
                               m SDL.Texture
renderCharOpaque renderer fnt color bkColor c =
    withUTF8 $ FNT.shadedGlyph fnt color bkColor c >>= renderInternal' renderer
{-# INLINE renderCharOpaque #-}

-- No exported.
drawInternal' :: MonadIO m => SDL.Renderer -> GuiPoint -> SDL.Texture -> m ()
drawInternal' renderer pnt texture = drawTexture renderer texture pnt >> SDL.destroyTexture texture
{-# INLINE drawInternal' #-}

-- No exported.
drawInternal :: MonadIO m => SDL.Renderer -> GuiPoint -> Maybe SDL.Texture -> m ()
drawInternal renderer pnt mbTexture = whenJust mbTexture (drawInternal' renderer pnt)
{-# INLINE drawInternal #-}

-- | Отрисовка строки с заданным шрифтом и цветом на указанном 'SDL.Renderer' в заданной точке.
-- Качественно, с полутонами, но медленно.
drawText :: MonadIO m => SDL.Renderer -> Font -> GuiColor -> GuiPoint -> T.Text -> m ()
drawText renderer fnt color pnt str = renderText renderer fnt color str >>= drawInternal renderer pnt
{-# INLINE drawText #-}

-- | Отрисовка строки с заданным шрифтом и цветом на указанном 'SDL.Renderer' в заданной точке.
-- Без полутонов, но быстро.
drawTextDraft :: MonadIO m => SDL.Renderer -> Font -> GuiColor -> GuiPoint -> T.Text -> m ()
drawTextDraft renderer fnt color pnt str = renderTextDraft renderer fnt color str >>= drawInternal renderer pnt
{-# INLINE drawTextDraft #-}

-- | Отрисовка строки с заданным шрифтом и цветом на указанном 'SDL.Renderer' в заданной точке.
-- С полутонами и быстро за счёт непрозрачного рисования по указанному фоновому цвету.
drawTextOpaque :: MonadIO m => SDL.Renderer -> -- ^ 'SDL.Renderer'.
                              Font -> -- ^ Шрифт.
                              GuiColor -> -- ^ Цвет текста.
                              GuiColor -> -- ^ Цвет фона.
                              GuiPoint -> -- ^ Позиция вывода.
                              T.Text -> -- ^ Выводимая строка.
                              m ()
drawTextOpaque renderer fnt color bkColor pnt str = renderTextOpaque renderer fnt color bkColor str
                                                            >>= drawInternal renderer pnt
{-# INLINE drawTextOpaque #-}

-- | Отрисовка одного символа с заданным шрифтом и цветом на указанном 'SDL.Renderer' в заданной точке.
-- С полутонами и быстро за счёт непрозрачного рисования по указанному фоновому цвету.
drawCharOpaque :: MonadIO m => SDL.Renderer -> -- ^ 'SDL.Renderer'.
                              Font -> -- ^ Шрифт.
                              GuiColor -> -- ^ Цвет текста.
                              GuiColor -> -- ^ Цвет фона.
                              GuiPoint -> -- ^ Позиция вывода.
                              Char -> -- ^ Выводимый символ.
                              m ()
drawCharOpaque renderer fnt color bkColor pnt c = renderCharOpaque renderer fnt color bkColor c
                                                            >>= drawInternal' renderer pnt
{-# INLINE drawCharOpaque #-}

-- | Режим отрисовки текста для универсальных функций вывода текста.
data DrawStrMode = DrawStrFine             -- ^ По прозрачному фону, качественно и медленно.
                 | DrawStrDraft            -- ^ По прозрачному фону, без полутонов но быстро.
                 | DrawStrOpaque GuiColor  -- ^ По заданному непрозрачному фону.
                 deriving (Eq, Show)

-- | Отрисовка строки с заданным шрифтом и цветом на указанном 'SDL.Renderer',
--  с заданным выравниванием в пределах заданного прямоугольника и с заданным режимом отрисовки.
drawStrAligned :: MonadIO m => SDL.Renderer -> -- ^ 'SDL.Renderer'.
                               Font -> -- ^ Шрифт.
                               Alignment -> -- ^ Режим выравнивания.
                               GuiColor -> -- ^ Цвет текста.
                               DrawStrMode -> -- ^ Режим отрисовки.
                               GuiRect -> -- ^ Прясоугольная область для вывода с учётом выравнивания.
                               T.Text -> -- ^ Выводимая строка.
                               m ()
drawStrAligned renderer fnt align color mode rect str = do
    mbTexture <- case mode of
            DrawStrFine -> renderText renderer fnt color str
            DrawStrDraft -> renderTextDraft renderer fnt color str
            DrawStrOpaque bkColor -> renderTextOpaque renderer fnt color bkColor str
    whenJust mbTexture $ \ t -> do
        drawTextureAligned renderer t align rect
        SDL.destroyTexture t

-- | Возвращает границы области отображаемой на дисплее, на котором отображается указанная точка
getDisplayRectByPoint :: MonadIO m => GuiPoint -> m (Maybe GuiRect)
getDisplayRectByPoint pnt =
    (listToMaybe . filter (`isInRect` pnt) .
            map (\d -> SDL.Rectangle (fromSDLPoint $ SDL.displayBoundsPosition d)
                            (fromSDLV2 $ SDL.displayBoundsSize d))) <$> SDL.getDisplays

