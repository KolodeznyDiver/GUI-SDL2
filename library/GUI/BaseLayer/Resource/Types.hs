module GUI.BaseLayer.Resource.Types(
    CachedItem,CacheCollection,TextureCollection,GuiFontDef(..),FontCollectionItem(..),FontCollection
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
type TextureCollection = CacheCollection SDL.Texture
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
        , textures  :: IORef TextureCollection
        , fonts     :: IORef FontCollection
        }

