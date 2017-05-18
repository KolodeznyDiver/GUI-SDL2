module GUI.BaseLayer.Skin where

--import qualified Data.Text as T
import GUI.BaseLayer.Types

data BorderType = BorderRound GuiColor
                | Border3D { border3DLightColor :: GuiColor, border3DDarkColor :: GuiColor }
                                    deriving (Show)

data DecoreState = DecoreState  { decoreBkColor   :: GuiColor
                                , decoreFgColor   :: GuiColor
                                }
                                deriving (Show)

data ButtonDecore = ButtonDecore    { btnDecoreOut :: DecoreState
                                    , btnDecoreIn  :: DecoreState
                                    , btnDecoreFocused  :: DecoreState
                                    , btnDecorePressed :: DecoreState
                                    , btnDecoreDisabled :: DecoreState
                                    , btnDecoreBorder :: BorderType
                                    }
                                    deriving (Show)


data Skin = Skin { skinName :: String
                 , mainBkColor :: GuiColor
                 , windowBkColor :: GuiColor
                 , bkColor :: GuiColor
                 , borderColor :: GuiColor
                 , widgetBorderColor :: GuiColor
                 , foregroundColor :: GuiColor
--                 , formItemsColor :: GuiColor
--                 , formItemsBkColor :: GuiColor
                 , formItemsMargin  :: WidgetMargin
--                 , labelsFontKey :: T.Text
--                 , buttonsFontKey :: T.Text
--                 , editFontKey :: T.Text
                 , formTextLineSpacing :: Double
                 , formItemsButtons :: ButtonDecore
                 , buttonsSpacing :: Coord
                 , disableFgColor  :: GuiColor
                 , scrollBarWidth  :: Coord
                 , arrowBtns  :: ButtonDecore
                 , scrollBarColor  :: GuiColor
                 , scrollBarSlider :: ButtonDecore
                 , scrollBarArrow :: ButtonDecore
                 , scrollAreaSlidersColor :: GuiColor
                 , scrollAreaArrowsColor :: GuiColor
                 , splitterActive :: DecoreState
                 }
