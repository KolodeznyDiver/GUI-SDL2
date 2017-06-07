{-# LANGUAGE StrictData #-}
module GUI.BaseLayer.Resource.Types(
    CachedItem,CacheCollection,GuiFontOptions(..),GuiFontDef(..),FontCollectionItem(..),FontCollection
    ,CursorCollection,ResourceManager(..)
                        ) where
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.IORef
import Data.Default
import qualified SDL
import SDL.TTF.FFI (TTFFont)
import SDL.TTF.Types
import GUI.BaseLayer.Cursor
import GUI.BaseLayer.Raw.TTF (GuiFontStyle(..))

type CachedItem a = a

type CacheCollection a = HM.HashMap T.Text (CachedItem a)
type SurfaceCollection = CacheCollection SDL.Surface

data GuiFontOptions = GuiFontOptions { fontKerning :: Maybe KerningStatus
                                     , fontHinting :: Maybe TTFHinting
                                     , fontStyle   :: Maybe GuiFontStyle
                                     }

instance Default GuiFontOptions where
    def = GuiFontOptions { fontKerning = Nothing
                         , fontHinting = Nothing
                         , fontStyle   = Nothing
                         }

data GuiFontDef = GuiFontDef { fontAbbrev    :: T.Text
                             , fontPath      :: String
                             , fontPtSz      :: Int
                             , fontOpts      :: GuiFontOptions
                             }

data FontCollectionItem = FontCollectionItem    { fntPath      :: String
                                                , fntPtSz      :: Int
                                                , fnt          :: TTFFont
                                                }
type FontCollection = CacheCollection FontCollectionItem
type CursorCollection = CacheCollection GuiCursor

data ResourceManager = ResourceManager
        { resPath   :: FilePath
        , skinPath  :: FilePath
        , systemCursorSet :: CursorSet
        , userCursors :: IORef CursorCollection
        , surfaces  :: IORef SurfaceCollection
        , fonts     :: IORef FontCollection
        }

