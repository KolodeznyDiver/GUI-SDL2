-- |
-- Module:      GUI.BaseLayer.Canvas.Types
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <kolodeznydiver@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Типы обеспечивающие функции рисования в 'Canvas'. Реализации функций см. "GUI.BaseLayer.Canvas".

module GUI.BaseLayer.Canvas.Types(
    TextureCache,CanvasRecord(..),Canvas
    ) where

import Data.IORef
import Control.Monad.Trans.Reader
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified SDL
import GUI.BaseLayer.Depend0.Types
import GUI.BaseLayer.Depend1.Resource (ResourceManager(..))


-- | Ссылка на кеш текстур (загруженных картинок) для конкретного окна.
-- Хранить приходится для каждого окна по отдельности - особенность SDL.
type TextureCache = IORef (HM.HashMap T.Text SDL.Texture)

-- | Информация для рисования и рендеринга текста, а так же использования менеджера ресурсов.
data CanvasRecord = CanvasRecord {
      canvasRenderer     :: SDL.Renderer -- ^ Renderer текущего окна.
    , canvasRM           :: ResourceManager -- ^ Менеджер ресурсов.
    , canvasTextureCache :: TextureCache -- ^ Ссылка на кеш текстур.
    , canvasOffset       :: GuiCoordOffset -- ^ Смещение координат текущего виджета.
                                 }

-- | Тип возвращаемого значения большинства функций "GUI.BaseLayer.Canvas", т.е. используемых в
-- @onDraw@ и функции, передаваемой к @GUI.BaseLayer.Core.runProxyCanvas@ и подобным.
type Canvas m a = ReaderT CanvasRecord m a


