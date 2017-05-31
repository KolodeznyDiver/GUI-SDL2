module GUI.BaseLayer.Skin where

--import qualified Data.Text as T
import GUI.BaseLayer.Types

data BtnBorderType = BtnBorderRound GuiColor
                   | BtnBorder3D    { btnBrdr3DLightColor :: GuiColor
                                    , btnBrdr3DDarkColor :: GuiColor
                                    }
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
                                    , btnDecoreBorder :: BtnBorderType
                                    }
                                    deriving (Show)


data Skin = Skin { skinName :: String
--                 , mainBkColor :: GuiColor
                 , windowBkColor :: GuiColor
                 , bkColor :: GuiColor
                 , borderColor :: GuiColor
                 , widgetBorderColor :: GuiColor
                 , foregroundColor :: GuiColor
                 , brdr3DLightColor :: GuiColor
                 , brdr3DDarkColor :: GuiColor
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
                 , popupMnuBkColor  :: GuiColor
                 , popupMnuInColor  :: GuiColor
                 , popupMnuFgColor  :: GuiColor
                 , popupMnuHotKeyColor  :: GuiColor
                 , popupMnuSeparatorColor  :: GuiColor
                 , popupMnu3DLightColor  :: GuiColor
                 , popupMnu3DDarkColor  :: GuiColor
                 }
