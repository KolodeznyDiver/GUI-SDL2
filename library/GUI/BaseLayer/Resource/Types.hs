{-# LANGUAGE StrictData #-}
module GUI.BaseLayer.Resource.Types(
    CachedItem,CacheCollection,GuiFontDef(..),FontCollectionItem(..),FontCollection
    ,CursorCollection,ResourceManager(..)
                        ) where
import qualified SDL
import SDL.TTF.FFI (TTFFont)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.IORef
import GUI.BaseLayer.Cursor

type CachedItem a = a

type CacheCollection a = HM.HashMap T.Text (CachedItem a)
type SurfaceCollection = CacheCollection SDL.Surface
data GuiFontDef = GuiFontDef { fontAbbrev    :: T.Text
                             , fontPath      :: String
                             , fontPtSz      :: Int
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

