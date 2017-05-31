{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-} -- BangPatterns Strict
module GUI.BaseLayer.Internal.Types(
    WidgetOpts,WindowOpts,WidgetFlags,WindowFlags,Widget,GuiWindow,Gui,WinTextureCache,Canvas(..),GuiCanvas
    ,UniqueCode(..),GuiNotifyCode(..),GuiPipeId(..),WidgetFunctions(..),SpecStateWidget(..)
    ,GuiWidgetCollection,GuiWindowCollection,UserMsgHandler(..),GuiPipeCollection
    ,WidgetStruct(..),WindowStruct(..),GUIStruct(..)
    ) where

import Control.Monad.Trans.Reader
import qualified SDL
import Data.Word
import GUI.BaseLayer.Types
import GUI.BaseLayer.BitFlags
import GUI.BaseLayer.Resource.Types (ResourceManager(..))
import GUI.BaseLayer.Skin
import GUI.BaseLayer.Cursor (CursorIx)
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.IntMap.Strict as IntMap
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T
import GUI.BaseLayer.Internal.Action
import Foreign.Ptr

data WidgetOpts
data WindowOpts
type WidgetFlags = Flags WidgetOpts
type WindowFlags = Flags WindowOpts
type Widget     = IORef WidgetStruct
type GuiWindow  = IORef WindowStruct
type Gui        = IORef GUIStruct
type WinTextureCache = IORef (HM.HashMap T.Text SDL.Texture)

data Canvas     = Canvas    { canvasRenderer        :: SDL.Renderer
                            , canvasRM              :: ResourceManager
                            , canvasTextureCache    :: WinTextureCache
                            , canvasOffset          :: GuiCoordOffset
                            }

type GuiCanvas m a = ReaderT Canvas m a

newtype UniqueCode = UniqueCode { unUniqueCode :: Int}
            deriving (Eq)

newtype GuiNotifyCode = GuiNotifyCode { unGuiNotifyCode :: Int}
            deriving (Eq)

newtype GuiPipeId = GuiPipeId { unGuiPipeId :: Int}
            deriving (Eq)

data WidgetFunctions = WidgetFunctions  { onCreate  :: forall m. MonadIO m => Widget -> m ()
                                        , onDestroy :: forall m. MonadIO m => Widget -> m ()
                                        , onDraw :: forall m. MonadIO m => Widget -> GuiCanvas m ()
                                        , onSizeChangedParentNotify :: forall m. MonadIO m =>
                                            Widget -> Widget -> GuiSize -> m ()
                                        , onMarkForRedrawNotiy :: forall m. MonadIO m => Widget -> m ()
                                        , onResizing :: forall m. MonadIO m => Widget -> GuiRect -> m ()
                                        , onGainedMouseFocus :: forall m. MonadIO m => Widget -> GuiPoint -> m ()
                                        , onMouseMotion :: forall m. MonadIO m =>
                                            Widget -> [SDL.MouseButton] -> GuiPoint -> GuiSize -> m ()
                                        , onMouseButton :: forall m. MonadIO m =>
                                            Widget -> SDL.InputMotion -> SDL.MouseButton -> Int -> GuiPoint -> m ()
                                        , onMouseWheel :: forall m. MonadIO m =>
                                            Widget -> GuiSize -> SDL.MouseScrollDirection -> m ()
                                        , onLostMouseFocus :: forall m. MonadIO m => Widget -> m ()
                                        , onGainedKeyboardFocus :: forall m. MonadIO m => Widget -> m ()
                                        , onLostKeyboardFocus :: forall m. MonadIO m => Widget -> m ()
                                        , onTextInput :: forall m. MonadIO m => Widget -> T.Text -> m ()
                                        , onKeyboard :: forall m. MonadIO m => Widget -> SDL.InputMotion -- Released | Pressed
                                                                             -> Bool -- True if this is a repeating key press from the user holding the key down.
                                                                             -> SDL.Keycode
                                                                             -> SDL.KeyModifier -- which keys are currently held down.
                                                                             -> m ()
                                        , onGuiStateChange :: forall m. MonadIO m => Widget -> GuiState -> m ()
                                        , onNotify :: forall m. MonadIO m => Widget -> GuiNotifyCode -> Maybe Widget -> m ()
                                        }

data SpecStateWidget    = WidgetNoSpecState
                        | WidgetStateMouseCaptured Widget

type GuiWidgetCollection = V.Vector Widget
type GuiWindowCollection = Map.Map GuiWindowIx GuiWindow

newtype UserMsgHandler = UserMsgHandler {userMsgHandler :: forall m. MonadIO m => Ptr () -> Ptr () -> m ()}

type GuiPipeCollection = IntMap.IntMap UserMsgHandler

data WidgetStruct = WidgetStruct    { windowOfWidget :: GuiWindow
                                    , parentWidget :: ~Widget
                                    , cildrenWidgets :: GuiWidgetCollection
                                    , widgetRect :: GuiRect
                                    , widgetCanvasRect :: GuiRect
                                    , widgetMargin :: GuiMargin
                                    , widgetFlags :: WidgetFlags
                                    , widgetCursor :: CursorIx
                                    , widgetFns :: WidgetFunctions
                                    }

data WindowStruct = WindowStruct    { guiOfWindow :: Gui
                                    , winSDL :: SDL.Window
                                    , winRenderer :: SDL.Renderer
                                    , mainWidget :: ~Widget
                                    , winFlags :: WindowFlags
                                    , specStateWidget :: SpecStateWidget
                                    , widgetUnderCursor :: Maybe Widget
                                    , focusedWidget :: Maybe Widget
                                    , curWinCursor :: CursorIx
                                    , winBuffer :: SDL.Texture
                                    , winProxyTexture :: SDL.Texture
                                    --, winPrev :: Maybe GuiWindow -- menu popup chain
                                    , winMainMenu :: Maybe Widget
                                    , winTextureCache :: WinTextureCache
                                    }

data GUIStruct = GUIStruct          { guiWindows :: GuiWindowCollection
                                    , guiSkin :: Skin
                                    , userEventCodeBase :: Word32
                                    , resourceManager :: ResourceManager
                                    , guiActions :: Actions
                                    , guiState :: GuiState
                                    , guiUnique :: Int
                                    , guiUserMsgHandlers :: GuiPipeCollection
                                    }

