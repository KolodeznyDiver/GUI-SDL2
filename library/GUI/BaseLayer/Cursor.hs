{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
module GUI.BaseLayer.Cursor(
     GuiCursor(..),CursorIx(..),pattern DefCursorIx,getCursor,setCursor
    ,activeCursor,freeCursor,createSystemCursor,CursorSet(..),freeCursorSet,mkSystemCursorSet,cursorFromSystemIx
                 ) where

--import qualified SDL
import qualified SDL.Raw as Raw
import Control.Monad.IO.Class (MonadIO)
--import Control.Monad
import Data.StateVar 
--import SDL.Exception 
import Foreign.Ptr
--import GUI.BaseLayer.Types
import qualified GUI.BaseLayer.Raw.Cursor as GuiRaw
import qualified Data.Text as T
--import qualified SDL.Input

newtype GuiCursor = GuiCursor { unwrapGuiCursor :: Raw.Cursor }
    deriving (Eq)

data CursorIx = SystemCursorArrow
              | SystemCursorIbeam
              | SystemCursorWait
              | SystemCursorCrossHair
              | SystemCursorWaitArrow
              | SystemCursorSizeNWSE
              | SystemCursorSizeNESW
              | SystemCursorSizeWE
              | SystemCursorSizeNS
              | SystemCursorSizeAll
              | SystemCursorNo
              | SystemCursorHand
              | CursorResourceIx T.Text
                    deriving (Eq)

pattern DefCursorIx :: CursorIx; pattern DefCursorIx = SystemCursorArrow

throwIfNull :: MonadIO m => T.Text -> Ptr a  -> m (Ptr a)
throwIfNull msg p = if p==nullPtr then
--                            SDL.showSimpleMessageBox Nothing SDL.Error "Error" msg >>
                         error $ T.unpack msg
                    else return p

getCursor :: MonadIO m => m GuiCursor
getCursor = GuiCursor <$> (GuiRaw.getCursor >>= throwIfNull "GUI.BaseLayer.Cursor.getCursor")

setCursor :: MonadIO m => GuiCursor -> m ()
setCursor = GuiRaw.setCursor . unwrapGuiCursor

activeCursor :: StateVar GuiCursor
activeCursor = makeStateVar getCursor setCursor

-- | Create a cursor using the specified bitmap data and mask (in MSB format).
--
--
{-
createCursor :: MonadIO m
             => V.Vector Bool -- ^ Whether this part of the cursor is black. Use 'False' for white and 'True' for black.
             -> V.Vector Bool -- ^ Whether or not pixels are visible. Use 'True' for visible and 'False' for transparent.
             -> V2 CInt -- ^ The width and height of the cursor.
             -> Point V2 CInt -- ^ The X- and Y-axis location of the upper left corner of the cursor relative to the actual mouse position
             -> m Cursor
createCursor dta msk (V2 w h) (P (V2 hx hy)) =
    liftIO . fmap Cursor $
        throwIfNull "SDL.Input.Mouse.createCursor" $
            V.unsafeWith (V.map (bool 0 1) dta) $ \unsafeDta ->
            V.unsafeWith (V.map (bool 0 1) msk) $ \unsafeMsk ->
                Raw.createCursor unsafeDta unsafeMsk w h hx hy
-}
-- | Free a cursor created with 'createCursor' and 'createColorCusor'.
--
-- See @<https://wiki.libsdl.org/SDL_FreeCursor SDL_FreeCursor>@ for C documentation.
freeCursor :: MonadIO m => GuiCursor -> m ()
freeCursor = GuiRaw.freeCursor . unwrapGuiCursor
 
createSystemCursor :: MonadIO m => Raw.SystemCursor -> m GuiCursor
createSystemCursor n = GuiCursor <$> Raw.createSystemCursor n


data CursorSet = CursorSet { arrowCursor :: GuiCursor
                           , ibeamCursor :: GuiCursor
                           , waitCursor :: GuiCursor
                           , crosshairCursor :: GuiCursor
                           , waitarrowCursor :: GuiCursor
                           , sizeNWSECursor :: GuiCursor
                           , sizeNESWCursor :: GuiCursor
                           , sizeWECursor :: GuiCursor
                           , sizeNSCursor :: GuiCursor
                           , sizeAllCursor :: GuiCursor
                           , noCursor :: GuiCursor
                           , handCursor :: GuiCursor
                           }


freeCursorSet ::  MonadIO m => CursorSet -> m ()
freeCursorSet (CursorSet{..}) = freeCursor arrowCursor >> freeCursor ibeamCursor >>
    freeCursor waitCursor >> freeCursor crosshairCursor >> freeCursor waitarrowCursor >>
    freeCursor sizeNWSECursor >> freeCursor sizeNESWCursor >> freeCursor sizeWECursor >>
    freeCursor sizeNSCursor >> freeCursor sizeAllCursor >> freeCursor noCursor >>
    freeCursor handCursor

mkSystemCursorSet:: MonadIO m => m CursorSet
mkSystemCursorSet = CursorSet <$> createSystemCursor Raw.SDL_SYSTEM_CURSOR_ARROW
                              <*> createSystemCursor Raw.SDL_SYSTEM_CURSOR_IBEAM
                              <*> createSystemCursor Raw.SDL_SYSTEM_CURSOR_WAIT
                              <*> createSystemCursor Raw.SDL_SYSTEM_CURSOR_CROSSHAIR
                              <*> createSystemCursor Raw.SDL_SYSTEM_CURSOR_WAITARROW
                              <*> createSystemCursor Raw.SDL_SYSTEM_CURSOR_SIZENWSE
                              <*> createSystemCursor Raw.SDL_SYSTEM_CURSOR_SIZENESW
                              <*> createSystemCursor Raw.SDL_SYSTEM_CURSOR_SIZEWE
                              <*> createSystemCursor Raw.SDL_SYSTEM_CURSOR_SIZENS
                              <*> createSystemCursor Raw.SDL_SYSTEM_CURSOR_SIZEALL
                              <*> createSystemCursor Raw.SDL_SYSTEM_CURSOR_NO
                              <*> createSystemCursor Raw.SDL_SYSTEM_CURSOR_HAND

cursorFromSystemIx :: CursorSet -> CursorIx -> GuiCursor
cursorFromSystemIx s SystemCursorArrow     = arrowCursor s
cursorFromSystemIx s SystemCursorIbeam     = ibeamCursor s
cursorFromSystemIx s SystemCursorWait      = waitCursor s
cursorFromSystemIx s SystemCursorCrossHair = crosshairCursor s
cursorFromSystemIx s SystemCursorWaitArrow = waitarrowCursor s
cursorFromSystemIx s SystemCursorSizeNWSE  = sizeNWSECursor s
cursorFromSystemIx s SystemCursorSizeNESW  = sizeNESWCursor s
cursorFromSystemIx s SystemCursorSizeWE    = sizeWECursor s
cursorFromSystemIx s SystemCursorSizeNS    = sizeNSCursor s
cursorFromSystemIx s SystemCursorSizeAll   = sizeAllCursor s
cursorFromSystemIx s SystemCursorNo        = noCursor s
cursorFromSystemIx s SystemCursorHand      = handCursor s
cursorFromSystemIx _ _ = undefined