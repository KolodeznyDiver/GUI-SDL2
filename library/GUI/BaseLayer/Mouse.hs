-- |
-- Module:      GUI.BaseLayer.Mouse
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Дополнение к пакету SDL2. Функции для работы с указателем мыши.

module GUI.BaseLayer.Mouse where

import Foreign.Marshal.Alloc
import Foreign.Storable
import Control.Monad.IO.Class
import SDL.Vect
import qualified SDL.Raw as Raw
import GUI.BaseLayer.Depend0.Types
import qualified GUI.BaseLayer.Primitives as P


-- | Retrieve the current location of the mouse in relation to the desktop
getDesktopMouseLocation :: MonadIO m => m GuiPoint
getDesktopMouseLocation = liftIO $
  alloca $ \x ->
  alloca $ \y -> do
    _ <- Raw.getGlobalMouseState x y
    (P . P.fromSDLV2) <$> (V2 <$> peek x <*> peek y)
