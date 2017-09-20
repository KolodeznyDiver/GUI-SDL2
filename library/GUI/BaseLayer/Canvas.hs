-- |
-- Module:      GUI.BaseLayer.Canvas
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Функции рисования в 'CanvasRecord' и связанные с ним, т.е. используемые в обработчике события
-- @GUI.BaseLayer.Types.onDraw@ а так же передаваемые фунуциям
-- @GUI.BaseLayer.Core.runProxyCanvas@,  @GUI.BaseLayer.Core.runProxyWinCanvas@
-- Большинство функций имеют сигнатуру @MonadIO m => .... -> Canvas m a@,
-- где 'Canvas' определено в "GUI.BaseLayer.Types" как
-- @type Canvas m a = ReaderT CanvasRecord m a@ и 'CanvasRecord' - запись, содержащая параметры отрисовки
-- в данном виджете, включая смещение виджета в клиентской области, ссылки на менеджер ресурсов и пр.

module GUI.BaseLayer.Canvas(
    -- * Создание 'Canvas'
    Canvas,runCanvas
    -- * Преобразование координат в и из CanvasRecord-контекста.
    ,toCanvasPoint,toCanvasRect,toSDLPoint,toSDLRect
    -- * Основные настройки режима рисования
    ,setColor,withColor,setBlendMode,withBlendMode,withClipRect
    -- * Рисование графических примитивов.
    ,drawPoint,drawPoints,drawLine,drawLines,drawRect,drawRects,fillRect,fillRects
    -- * Текстуры.
    ,getTextureSize
    -- ** Рисование произвольных текстур.
    ,drawTexture,drawStretchedTexture,drawTexturePartial,drawTextureAligned,drawTextureEx
    -- ** Текстуры из графических файлов (кешируемые).
    -- Предупреждение : При загрузки некоторых графических файлов в консоль/терминал могут выдаваться сообщения
    -- __/\"libpng warning: iCCP: known incorrect sRGB profile\"/__.
    -- Такие файлы можно исправить, см. <https://stackoverflow.com/questions/22745076/libpng-warning-iccp-known-incorrect-srgb-profile>
    ,getTexture,drawTextureR,drawStretchedTextureR,drawTexturePartialR,drawTextureAlignedR,drawTextureExR
    -- ** Прочие функции относящиеся к текстурам.
    ,createTargetTexture,withTargetTexture,withTransparentTexture
    -- * Отрисовка текста
    ,getFont,DrawStrMode(..)
    -- ** Строки 'T.Text'
    ,getTextSize,renderText,renderTextDraft,renderTextOpaque,renderCharOpaque
    ,drawText,drawTextDraft,drawTextOpaque,drawCharOpaque,drawTextAligned
    -- * Специализированные функции рисования
    ,drawRoundBorder,drawRoundFrame,draw3DBorder,draw3DFrame,drawDotBorder,Orientation(..),drawArrowTriangle,drawArrow
                     ) where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import qualified SDL
import qualified SDL.Raw as Raw
import SDL.Internal.Types
import SDL.Vect
import Data.StateVar
import SDL.Font
import Control.Monad
import Control.Monad.IO.Class -- (MonadIO)
import qualified Data.Vector.Storable as V
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import GUI.BaseLayer.Depend0.Types
import GUI.BaseLayer.Depend0.Ref
import GUI.BaseLayer.Depend1.Geometry
import qualified GUI.BaseLayer.Primitives as P
import           GUI.BaseLayer.Primitives (DrawStrMode(..))
import GUI.BaseLayer.Canvas.Types
import GUI.BaseLayer.Resource

-- | Создание 'Canvas' и вызова функции выполняющийся в её контексте.
-- Не использовать вне /GUI.BaseLayer/. Для досупа к функциям данного модуля вне обработчика
-- @GUI.BaseLayer.Types.onDraw@ используйте
-- @GUI.BaseLayer.Core.runProxyCanvas@,  @GUI.BaseLayer.Core.runProxyWinCanvas@.
runCanvas :: MonadIO m => SDL.Renderer -> ResourceManager -> TextureCache -> GuiCoordOffset ->
                                    Canvas m a -> m a
runCanvas renderer rm cache off f = runReaderT f $ CanvasRecord renderer rm cache off
{-# INLINE runCanvas #-}

-- | Преобразование координат точки в виджете в координаты канвы (обычно координаты клиентской области окна).
toCanvasPoint :: CanvasRecord -> GuiPoint -> GuiPoint
toCanvasPoint c p = p .+^ canvasOffset c
{-# INLINE toCanvasPoint #-}

-- | Преобразование координат прямоугольника в виджете в координаты канвы (обычно координаты клиентской области окна).
toCanvasRect ::  CanvasRecord -> GuiRect -> GuiRect
toCanvasRect c (SDL.Rectangle p sz) = SDL.Rectangle (toCanvasPoint c p) sz
{-# INLINE toCanvasRect #-}

-- | Преобразование координат точки в виджете в SDL координаты.
-- Отлдичается от @toCanvasPoint@ только другим интегральным типом задающим координаты.
toSDLPoint :: CanvasRecord -> GuiPoint -> SDL.Point V2 SDLCoord
toSDLPoint c p = fromIntegral <$> toCanvasPoint c p
{-# INLINE toSDLPoint #-}

-- | Преобразование координат прямоугольника в виджете в SDL координаты.
-- Отлдичается от @toCanvasRect@ только другим интегральным типом задающим координаты.
toSDLRect :: CanvasRecord -> GuiRect -> SDL.Rectangle SDLCoord
toSDLRect c = fmap fromIntegral . toCanvasRect c
{-# INLINE toSDLRect #-}

-- | Установить текущий цвет рисования для графических примитивов (не для функций отрисовки шрифтов).
setColor  :: MonadIO m => GuiColor -> Canvas m ()
setColor (V4 r g b a) = do
    (SDL.Internal.Types.Renderer re) <- asks canvasRenderer
    void $ lift $ Raw.setRenderDrawColor re r g b a
{-# INLINE setColor #-}

-- | Установить текущий цвет рисования для графических примитивов (не для функций отрисовки шрифтов)
-- Временно, для переданной вторым аргументов функции. После её выполнения цвет восстанавливается.
withColor :: MonadIO m => GuiColor -> Canvas m a -> Canvas m a
withColor color a = do { renderer <- asks canvasRenderer; withStateVar (SDL.rendererDrawColor renderer) color a}
{-# INLINE withColor #-}

-- | Вспомогательная функция сохраняющая значение типа __/a/__ с помощью @StateVar a@,
-- устанавливающая заданное значение, и, после выполнения переданной ей функции
-- восстанавливающая сохранённое значение. В "GUI.BaseLayer.Primitives" есть аналогичная функция,
-- но не предполагающая контекст 'Canvas'.
withStateVar :: MonadIO m => StateVar a -> a -> Canvas m b -> Canvas m b
withStateVar sv a b = do {save <- lift $ get sv; lift (sv $= a); r <- b; lift (sv $= save); return r}
{-# INLINE withStateVar #-}

-- | Установить текущий режим прозрачности для графических примитивов (не для текстур и шрифтов).
-- Уровень прозрачности определяется четвёртой компонентоы заданного цвета.
setBlendMode :: MonadIO m => SDL.BlendMode -> Canvas m ()
setBlendMode m = do { renderer <- asks canvasRenderer; lift (SDL.rendererDrawBlendMode renderer $= m)}
{-# INLINE setBlendMode #-}

-- | Установить текущий режим прозрачности для графических примитивов (не для текстур и шрифтов).
-- Уровень прозрачности определяется четвёртой компонентоы заданного цвета.
-- Режим прозрачности устанавливается временно, пока выполняется функция переданная вторым аргументом.
withBlendMode :: MonadIO m => SDL.BlendMode -> Canvas m a -> Canvas m a
withBlendMode m a = do { renderer <- asks canvasRenderer; withStateVar (SDL.rendererDrawBlendMode renderer) m a}
{-# INLINE withBlendMode #-}

-- | На время выполнения переданной вторым аргументов функции устанавливается указанный
-- прямоугольник отсечения. Отриосвка за его пределами обрезается.
-- Прямоугольник отсечения не может быть задан больше чем уже установленный для виджета перед вызовом
-- @GUI.BaseLayer.Types.onDraw@, иначе он будет обрезан что бы не допустить,
-- чтобы виджет рисовал не в своей области.
withClipRect :: MonadIO m => GuiRect -> Canvas m a -> Canvas m a
withClipRect rect f = do
    c@CanvasRecord{canvasRenderer=renderer} <- ask
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

-- | Нарисовать точку
drawPoint :: MonadIO m => GuiPoint -> Canvas m ()
drawPoint p = do{ c <- ask; lift $ SDL.drawPoint (canvasRenderer c) $ toSDLPoint c p}
{-# INLINE drawPoint #-}

-- | Нарисовать несколько точек.
drawPoints :: MonadIO m => V.Vector GuiPoint -> Canvas m ()
drawPoints v = do{ c <- ask; lift $ SDL.drawPoints (canvasRenderer c) $ V.map (toSDLPoint c) v}
{-# INLINE drawPoints #-}

-- | Нарисовать линию.
drawLine :: MonadIO m => GuiPoint -> GuiPoint -> Canvas m ()
drawLine p0 p1 =  do { c <- ask; lift $ SDL.drawLine (canvasRenderer c) (toSDLPoint c p0) (toSDLPoint c p1)}
{-# INLINE drawLine #-}

-- | Нарисовать ломаную линию (незамкнутую).
drawLines :: MonadIO m => V.Vector GuiPoint -> Canvas m ()
drawLines v = do{ c <- ask; lift $ SDL.drawLines (canvasRenderer c) $ V.map (toSDLPoint c) v}

-- | Нарисовать незакрашенный прямоугольник.
drawRect :: MonadIO m => GuiRect -> Canvas m ()
drawRect r = do{ c <- ask; lift $ SDL.drawRect (canvasRenderer c) $ Just $ toSDLRect c r}
{-# INLINE drawRect #-}

-- | Нарисовать несколько незакрашенных прямоугольников.
drawRects :: MonadIO m => V.Vector GuiRect -> Canvas m ()
drawRects v = do{ c <- ask; lift $ SDL.drawRects (canvasRenderer c) $ V.map (toSDLRect c) v}
{-# INLINE drawRects #-}

-- | Нарисовать незакрашенный прямоугольник.
fillRect :: MonadIO m => GuiRect -> Canvas m ()
fillRect r = do{ c <- ask; lift $ SDL.fillRect (canvasRenderer c) $ Just $ toSDLRect c r}
{-# INLINE fillRect #-}

-- | Нарисовать несколько закрашенных прямоугольников.
fillRects :: MonadIO m => V.Vector GuiRect -> Canvas m ()
fillRects v = do{ c <- ask; lift $ SDL.fillRects (canvasRenderer c) $ V.map (toSDLRect c) v}
{-# INLINE fillRects #-}

-- | Возвращает размеры текстуры.
getTextureSize :: MonadIO m => SDL.Texture -> Canvas m GuiSize
getTextureSize = lift . P.getTextureSize
{-# INLINE getTextureSize #-}

-- | Нарисовать заданную текстуру в заданной точке (задаётся положение левого верхнего угла текстуры).
drawTexture :: MonadIO m => SDL.Texture -> GuiPoint -> Canvas m ()
drawTexture texture pnt =  do{ c <- ask; lift $ P.drawTexture (canvasRenderer c) texture $ toCanvasPoint c pnt}
{-# INLINE drawTexture #-}

-- | Нарисовать заданную текстуру или часть текстуры в заданном прямоугольнике, возможно с изменением размера.
drawStretchedTexture  :: MonadIO m => SDL.Texture -> -- ^ Текстура
                                      Maybe GuiRect -> -- ^ Отрисовываемя часть текстуры в координатах текстуры
                                                       -- или вся текстура ('Nothing').
                                      GuiRect -> -- ^ Область отрисовки вкоординатах виджета.
                                      Canvas m ()
drawStretchedTexture t src dst = do
    c <- ask
    lift $ SDL.copy (canvasRenderer c) t (fmap P.toSDLRect src) $ Just $ toSDLRect c dst
{-# INLINE drawStretchedTexture #-}

-- | Нарисовать часть текстуры в заданной точке, без изменения размера.
drawTexturePartial  :: MonadIO m => SDL.Texture -> -- ^ Текстура
                                    GuiRect -> -- ^ Отрисовываемя часть текстуры в координатах текстуры.
                                    GuiPoint -> -- ^ Выходное положение текстуры в координатах виджета.
                                    Canvas m ()
drawTexturePartial t src@(SDL.Rectangle _ sz) dst = do
    c <- ask
    lift $ SDL.copy (canvasRenderer c) t (Just $ P.toSDLRect src) $ Just
        $ SDL.Rectangle (toSDLPoint c dst) $ P.toSDLV2 sz
{-# INLINE drawTexturePartial #-}

-- | Нарисовать заданную текстуру с указанным выравниванием в указанном прямоугольнике
-- (координаты виджета), без изменения размера. Обычно прямоугольник указывается больше размеров текстуры.
drawTextureAligned :: MonadIO m => SDL.Texture -> Alignment -> GuiRect -> Canvas m ()
drawTextureAligned texture align rect =  do
    c <- ask
    lift $ P.drawTextureAligned (canvasRenderer c) texture align $ toCanvasRect c rect
{-# INLINE drawTextureAligned #-}

-- | Нарисовать заданную текстуру или часть текстуры в заданном прямоугольнике,
-- возможно с изменением размера, с возможностью вращения и отражений.
drawTextureEx :: MonadIO m => SDL.Texture -> -- ^ Текстура
                              Maybe GuiRect -> -- ^ Отрисовываемя часть текстуры в координатах текстуры
                                               -- или вся текстура ('Nothing').
                              GuiRect -> -- ^ Область отрисовки вкоординатах виджета.
                              Double -> -- ^ угол попорота, в градусах, по часовой, обасти орисовки.
                              Maybe GuiPoint -> -- ^ точка вращения или центр области отрисовки, если Nothing.
                              Bool -> -- ^ Отразить по вертикали.
                              Bool -> -- ^ Отразить по горизонтали.
                              Canvas m ()
drawTextureEx texture src dst rotateAngle rotPnt flipV flipH =  do
    c <- ask
    lift $ SDL.copyEx (canvasRenderer c) texture (fmap P.toSDLRect src)
        (Just $ toSDLRect c dst)  (realToFrac rotateAngle) (fmap (toSDLPoint c) rotPnt) $ V2 flipV flipH
{-# INLINE drawTextureEx #-}

-- | Загрузить текстуру из графического файла (см. "GUI.BaseLayer.Resource").
 -- Текстура остаётся в кеше. Удалять её вызовом @SDL.destroyTexture@ нельзя.
getTexture :: MonadIO m => T.Text -> Canvas m SDL.Texture
getTexture k = do
    c <- ask
    cache <- readMonadIORef $ canvasTextureCache c
    case HM.lookup k cache of
        Just t -> return t
        _ -> do
            t <- lift (rmGetSurface (canvasRM c) k >>= SDL.createTextureFromSurface (canvasRenderer c))
            writeMonadIORef (canvasTextureCache c) $ HM.insert k t cache
            return t

-- | Как @drawTexture@, но текстура задаётся ключом - именем графического файла.
drawTextureR :: MonadIO m => T.Text -> GuiPoint -> Canvas m ()
drawTextureR k p = do { t <- getTexture k; drawTexture t p}
{-# INLINE drawTextureR #-}

-- | Как @drawStretchedTexture@, но текстура задаётся ключом - именем графического файла.
drawStretchedTextureR  :: MonadIO m => T.Text -> Maybe GuiRect -> GuiRect -> Canvas m ()
drawStretchedTextureR k src dst = do { t <- getTexture k; drawStretchedTexture t src dst}
{-# INLINE drawStretchedTextureR #-}

-- | Как @drawTexturePartial@, но текстура задаётся ключом - именем графического файла.
drawTexturePartialR  :: MonadIO m => T.Text -> GuiRect -> GuiPoint -> Canvas m ()
drawTexturePartialR k src p = do { t <- getTexture k; drawTexturePartial t src p}
{-# INLINE drawTexturePartialR #-}

-- | Как @drawTextureAligned@, но текстура задаётся ключом - именем графического файла.
drawTextureAlignedR :: MonadIO m => T.Text -> Alignment -> GuiRect -> Canvas m ()
drawTextureAlignedR k align rect = do { t <- getTexture k; drawTextureAligned t align rect}
{-# INLINE drawTextureAlignedR #-}

-- | Как @drawTextureEx@, но текстура задаётся ключом - именем графического файла.
drawTextureExR :: MonadIO m => T.Text -> Maybe GuiRect -> GuiRect -> Double -> Maybe GuiPoint
                    -> Bool -> Bool -> Canvas m ()
drawTextureExR k src dst rotateAngle rotPnt flipV flipH =
    do { t <- getTexture k; drawTextureEx t src dst rotateAngle rotPnt flipV flipH}
{-# INLINE drawTextureExR #-}


-- | Создаёт текстуру совместимую с окном текущего виджета, заданного размера, которая может
-- быть буфером для создания изображения для окна виджета.
createTargetTexture :: MonadIO m => GuiSize -> Canvas m SDL.Texture
createTargetTexture sz = do { renderer <- asks canvasRenderer; lift $ P.createTargetTexture renderer sz}
{-# INLINE createTargetTexture #-}

-- | В функции, переданной вторым аргументом, отрисовка выполняется в указанную текстуру, а не буфер окна.
-- Часто используется не в обработчиках @GUI.BaseLayer.Types.onDraw@, а в функиях
-- @GUI.BaseLayer.Core.runProxyCanvas@,  @GUI.BaseLayer.Core.runProxyWinCanvas@ для подготовки текстур
-- при инициализации виджета и т.п.
withTargetTexture :: MonadIO m => SDL.Texture -> Canvas m a -> Canvas m a
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
            textureSz <- getTextureSize t
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

-- | В функции, переданной третьим аргументом, отрисовка текстуры выполняется с заданной прозрачностью.
withTransparentTexture :: MonadIO m => GuiTransparency -> -- ^ Прозрачность 0..255.
                                       SDL.Texture -> -- ^ Текстура.
                                       Canvas m a -> -- ^ Функция в которой указанная текстура может
                                                        -- быть прозрачна.
                                       Canvas m a
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

-- | Получить шрифт из кеша по ключу. Исключение если таблица шрифтов пуста.
-- См. "GUI.BaseLayer.Resource".
getFont:: MonadIO m => T.Text -> Canvas m Font
getFont k = do { rm <- asks canvasRM; lift $ rmGetFont rm k}
{-# INLINE getFont #-}

------------------------------------------------------------------------------------------------------

-- | Возвращает размер который будет занимать на экране заданная строка выведенная заданным шрифтом.
getTextSize :: MonadIO m => Font -> T.Text -> Canvas m (V2 Coord)
getTextSize fnt = lift . P.textSize fnt
{-# INLINE getTextSize #-}

-- | Создание текстуры из строки с заданным шрифтом и цветом.
-- Качественно, с полутонами, но медленно.
renderText:: MonadIO m => Font -> GuiColor -> T.Text -> Canvas m (Maybe SDL.Texture)
renderText fnt color txt = do { renderer <- asks canvasRenderer; lift $ P.renderText renderer fnt color txt}
{-# INLINE renderText #-}

-- | Создание текстуры из строки с заданным шрифтом и цветом.
-- Без полутонов, но быстро.
renderTextDraft:: MonadIO m => Font -> GuiColor -> T.Text -> Canvas m (Maybe SDL.Texture)
renderTextDraft fnt color txt = do { renderer <- asks canvasRenderer; lift $ P.renderTextDraft renderer fnt color txt}
{-# INLINE renderTextDraft #-}

-- | Создание текстуры из строки с заданным шрифтом и цветом.
-- С полутонами и быстро за счёт непрозрачного рисования по указанному фоновому цвету.
renderTextOpaque:: MonadIO m => Font -> -- ^ Шрифт.
                                GuiColor -> -- ^ Цвет текста.
                                GuiColor -> -- ^ Цвет фона.
                                T.Text -> -- ^ Выводимая строка.
                                Canvas m (Maybe SDL.Texture)
renderTextOpaque fnt color bkColor txt = do
    renderer <- asks canvasRenderer
    lift $ P.renderTextOpaque renderer fnt color bkColor txt
{-# INLINE renderTextOpaque #-}

-- | Создание текстуры из из одного символа с заданным шрифтом и цветом.
-- С полутонами и быстро за счёт непрозрачного рисования по указанному фоновому цвету.
renderCharOpaque:: MonadIO m => Font -> -- ^ Шрифт.
                                GuiColor -> -- ^ Цвет текста.
                                GuiColor -> -- ^ Цвет фона.
                                Char -> -- ^ Выводимый символ.
                                Canvas m SDL.Texture
renderCharOpaque fnt color bkColor c = do
    renderer <- asks canvasRenderer
    lift $ P.renderCharOpaque renderer fnt color bkColor c
{-# INLINE renderCharOpaque #-}

-- | Отрисовка строки с заданным шрифтом и цветом в заданной точке.
-- Качественно, с полутонами, но медленно.
drawText :: MonadIO m => Font -> GuiColor -> GuiPoint -> T.Text -> Canvas m ()
drawText fnt color pnt txt = do{ c <- ask; lift $ P.drawText (canvasRenderer c) fnt color (toCanvasPoint c pnt) txt}
{-# INLINE drawText #-}

-- | Отрисовка строки с заданным шрифтом и цветом в заданной точке.
-- Без полутонов, но быстро.
drawTextDraft :: MonadIO m => Font -> GuiColor -> GuiPoint -> T.Text -> Canvas m ()
drawTextDraft fnt color pnt txt =  do
    c <- ask
    lift $ P.drawTextDraft (canvasRenderer c) fnt color (toCanvasPoint c pnt) txt
{-# INLINE drawTextDraft #-}

-- | Отрисовка строки с заданным шрифтом и цветом в заданной точке.
-- С полутонами и быстро за счёт непрозрачного рисования по указанному фоновому цвету.
drawTextOpaque :: MonadIO m => Font -> -- ^ Шрифт.
                               GuiColor -> -- ^ Цвет текста.
                               GuiColor -> -- ^ Цвет фона.
                               GuiPoint -> -- ^ Позиция вывода.
                               T.Text -> -- ^ Выводимая строка.
                               Canvas m ()
drawTextOpaque fnt color bkColor pnt txt = do
    c <- ask
    lift $ P.drawTextOpaque (canvasRenderer c) fnt color bkColor (toCanvasPoint c pnt) txt
{-# INLINE drawTextOpaque #-}

-- | Отрисовка одного символа с заданным шрифтом и цветом в заданной точке.
-- С полутонами и быстро за счёт непрозрачного рисования по указанному фоновому цвету.
drawCharOpaque :: MonadIO m => Font -> -- ^ Шрифт.
                               GuiColor -> -- ^ Цвет текста.
                               GuiColor -> -- ^ Цвет фона.
                               GuiPoint -> -- ^ Позиция вывода.
                               Char -> -- ^ Выводимый символ.
                               Canvas m ()
drawCharOpaque fnt color bkColor pnt c' = do
    c <- ask
    lift $ P.drawCharOpaque (canvasRenderer c) fnt color bkColor (toCanvasPoint c pnt) c'
{-# INLINE drawCharOpaque #-}

-- | Отрисовка строки с заданным шрифтом и цветом,
--  с заданным выравниванием в пределах заданного прямоугольника и с заданным режимом отрисовки.
drawTextAligned :: MonadIO m => Font -> -- ^ Шрифт.
                                Alignment -> -- ^ Режим выравнивания.
                                GuiColor -> -- ^ Цвет текста.
                                DrawStrMode -> -- ^ Режим отрисовки.
                                GuiRect -> -- ^ Прясоугольная область для вывода с учётом выравнивания.
                                T.Text -> -- ^ Выводимая строка.
                                Canvas m ()
drawTextAligned fnt align color mode rect txt = do
    c <- ask
    lift $  P.drawStrAligned (canvasRenderer c) fnt align color mode (toCanvasRect c rect) txt
{-# INLINE drawTextAligned #-}
------------------------------------------------------------------------------------------------------

-- | Нарисовать прямоугольник с закруглёнными (на самом деле скошенными, но это мало заметно)
-- краями из тонкой линии текущего установленного цвета.
drawRoundBorder :: MonadIO m => GuiRect -> Canvas m ()
drawRoundBorder (SDL.Rectangle p (V2 w' h')) =
    let radius = 2
        w = w' - 1
        h = h' - 1 in do
      drawLine (p .+^ V2 0 radius) (p .+^ V2 0 (h-radius))
      drawLine (p .+^ V2 w radius) (p .+^ V2 w (h-radius))
      drawLine (p .+^ V2 radius 0) (p .+^ V2 (w-radius) 0)
      drawLine (p .+^ V2 radius h) (p .+^ V2 (w-radius) h)
      drawPoints $ V.fromList [p .+^ V2 1 1,p .+^ V2 1 (h-1),p .+^ V2 (w-1) 1,p .+^ V2 (w-1) (h-1)]
{-
    drawLines $ V.fromList [
        p .+^ V2 0 radius, p .+^ V2 radius 0,
        p .+^ V2 (w-radius) 0, p .+^ V2 w radius,
        p .+^ V2 w (h-radius), p .+^ V2 (w-radius) h,
        p .+^ V2 radius h, p .+^ V2 0 (h-radius),    p .+^ V2 0 radius  ]
-}

-- | Нарисовать прямоугольник с закруглёнными краями.
drawRoundFrame :: MonadIO m =>
--                  GuiColor -> -- ^ Цвет вне прямоугольника. Нужен для восстановления закруглений.
                  GuiColor -> -- ^ Цвет рамки - границы прямоугольника.
                  GuiColor -> -- ^ Цвет внутри прямоугольника.
                  GuiRect -> -- ^ Границы прямоугольника.
                  Canvas m ()
drawRoundFrame borderColor insideColor r{-@(SDL.Rectangle p (V2 w' h'))-} = do
    setColor insideColor
    fillRect $ shrinkRect' 1 r
{-    setColor outsideColor
    let w = w' - 1
        h = h' - 1
    drawPoints $ V.concat [cornPnts 1 1 p, cornPnts  (-1) 1 $ p .+^ V2 w 0,
        cornPnts (-1) (-1) $ p .+^ V2 w h, cornPnts 1 (-1) $ p .+^ V2 0 h] -}
    setColor borderColor
    drawRoundBorder r
{-  where cornPnts :: Coord -> Coord -> GuiPoint -> V.Vector GuiPoint
        cornPnts dx dy pt = V.fromList [pt, pt .+^ V2 dx 0, pt .+^ V2 0 dy] -}

-- | Нарисовать псевдо 3D прямоугольную рамку без внутреннего заполнения.
draw3DBorder :: MonadIO m => GuiColor -> -- ^ Цвет левого и верхнего края.
                             GuiColor -> -- ^ Цвет правого и нижнего края.
                             Int -> -- ^ Толщина рамки, пикселей.
                             GuiRect -> -- ^ Внешние границы рамки.
                             Canvas m ()
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

-- | Нарисовать псевдо 3D прямоугольную рамку с внутренним заполнением.
draw3DFrame :: MonadIO m => GuiColor -> -- ^ Цвет левого и верхнего края.
                            GuiColor -> -- ^ Цвет правого и нижнего края.
                            GuiColor -> -- ^ Цвет внутреннего заполнения.
                            Int -> -- ^ Толщина рамки, пикселей.
                            GuiRect -> -- ^ Внешние границы рамк
                            Canvas m ()
draw3DFrame lightColor darkColor insideColor thickness r = do
    setColor insideColor
    fillRect r
    draw3DBorder lightColor darkColor thickness r

-- | Нарисовать прямоугольник
drawDotBorder :: MonadIO m => Coord -> GuiRect -> Canvas m ()
drawDotBorder step = drawPoints . mkDotRectVector step
{-# INLINE drawDotBorder #-}

-- | Ориентация
data Orientation =  OrientationLeft  -- ^ Влево.
                 | OrientationUp     -- ^ Вверх.
                 | OrientationRight  -- ^ Вправо.
                 | OrientationDown   -- ^ Вниз.
                 deriving (Eq, Show)

-- | Нарисовать почти равносторонний треугольник (относительно медленно).
drawArrowTriangle :: MonadIO m => Orientation ->  -- ^ Ориентация одного из углов.
                                  GuiColor -> -- ^ Цвет.
                                  GuiPoint -> -- ^ Центральная точка.
                                  Coord ->  -- ^ Высота.
                                  Canvas m ()
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

-- | Нарисовать почти равносторонний треугольник только линиями. Быстро. в основании остаётся
-- треугольная \"впуклость\" которую принимаем за фичу.
-- Используется текущий цвет.
drawArrow :: MonadIO m => Orientation -> -- ^ Ориентация одного из углов.
                          GuiPoint -> -- ^ Центральная точка.
                          Coord ->  -- ^ Высота.
                          Canvas m ()
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


