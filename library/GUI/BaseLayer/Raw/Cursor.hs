module GUI.BaseLayer.Raw.Cursor(createCursor,freeCursor,getCursor,setCursor
    )
 where

import qualified SDL.Raw.Types as Raw
import Foreign.C.Types
import Foreign.Ptr
import Control.Monad.IO.Class
import Data.Word

--foreign import ccall "SDL.h SDL_CreateColorCursor" createColorCursorFFI :: Ptr Surface -> CInt -> CInt -> IO SDL.Raw.Types.Cursor
foreign import ccall "SDL.h SDL_CreateCursor" createCursorFFI :: Ptr Word8 -> Ptr Word8 -> CInt -> CInt -> CInt -> CInt -> IO Raw.Cursor
--foreign import ccall "SDL.h SDL_CreateSystemCursor" createSystemCursorFFI :: SystemCursor -> IO Raw.Cursor
foreign import ccall "SDL.h SDL_FreeCursor" freeCursorFFI :: Raw.Cursor -> IO ()
foreign import ccall "SDL.h SDL_GetCursor" getCursorFFI :: IO Raw.Cursor
--foreign import ccall "SDL.h SDL_GetDefaultCursor" getDefaultCursorFFI :: IO Raw.Cursor
--foreign import ccall "SDL.h SDL_GetMouseFocus" getMouseFocusFFI :: IO Window
--foreign import ccall "SDL.h SDL_GetMouseState" getMouseStateFFI :: Ptr CInt -> Ptr CInt -> IO Word32
--foreign import ccall "SDL.h SDL_GetRelativeMouseMode" getRelativeMouseModeFFI :: IO Bool
--foreign import ccall "SDL.h SDL_GetRelativeMouseState" getRelativeMouseStateFFI :: Ptr CInt -> Ptr CInt -> IO Word32
foreign import ccall "SDL.h SDL_SetCursor" setCursorFFI :: Raw.Cursor -> IO ()
--foreign import ccall "SDL.h SDL_SetRelativeMouseMode" setRelativeMouseModeFFI :: Bool -> IO CInt
--foreign import ccall "SDL.h SDL_ShowCursor" showCursorFFI :: CInt -> IO CInt
--foreign import ccall "SDL.h SDL_WarpMouseInWindow" warpMouseInWindowFFI :: Window -> CInt -> CInt -> IO ()

createCursor :: MonadIO m => Ptr Word8 -> Ptr Word8 -> CInt -> CInt -> CInt -> CInt -> m Raw.Cursor
createCursor v1 v2 v3 v4 v5 v6 = liftIO $ createCursorFFI v1 v2 v3 v4 v5 v6
{-# INLINE createCursor #-}
{-
createSystemCursor :: MonadIO m => SystemCursor -> m Raw.Cursor
createSystemCursor v1 = liftIO $ createSystemCursorFFI v1
{- INLINE createSystemCursor -}
-}
freeCursor :: MonadIO m => Raw.Cursor -> m ()
freeCursor v1 = liftIO $ freeCursorFFI v1
{-# INLINE freeCursor #-}

getCursor :: MonadIO m => m Raw.Cursor
getCursor = liftIO getCursorFFI
{-# INLINE getCursor #-}

setCursor :: MonadIO m => Raw.Cursor -> m ()
setCursor v1 = liftIO $ setCursorFFI v1
{-# INLINE setCursor #-}
