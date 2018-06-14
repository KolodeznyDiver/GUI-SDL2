-- |
-- Module:      GUI.BaseLayer.Depend0.SDLWindow
-- Copyright:   (c) 2017-2018 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Дополнение к пакету SDL2. Функции для работы с окном на уровне SDL.
-- Функция @SDL_GetWindowBordersSize@ поддерживается только начиная с версии SDL 2.0.5, и пока только под X11,
-- по этому данный модуль временно исключён из пакета.

module GUI.BaseLayer.Depend0.SDLWindow(
       -- * Типы
       SDLWindowBorders(..)
       -- * Функции
       ,getWindowBordersSize 
       ) where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Control.Monad.IO.Class
import qualified SDL
import qualified SDL.Raw as Raw
import qualified SDL.Internal.Types

foreign import ccall "SDL.h SDL_GetWindowBordersSize" 
   getWindowBordersSizeFFI :: Raw.Window -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()

data SDLWindowBorders = SDLWindowBorders {
        brdrTop :: Int
      , brdrLeft :: Int
      , brdrBottom :: Int
      , brdrRight :: Int
                                         }
      deriving(Eq,Show)

-- | Retrieve the window's borders (decorations) around the client area
getWindowBordersSize :: MonadIO m => SDL.Window -> m SDLWindowBorders
getWindowBordersSize (SDL.Internal.Types.Window win) = liftIO $
  alloca $ \t ->
  alloca $ \l ->
  alloca $ \b ->
  alloca $ \r -> do
    _ <- getWindowBordersSizeFFI win t l b r
    let peek' = fmap fromIntegral . peek
    SDLWindowBorders <$> peek' t <*> peek' l <*> peek' b <*> peek' r

