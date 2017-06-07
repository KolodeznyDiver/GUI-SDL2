{-# LANGUAGE OverloadedStrings #-}
module GUI.Skin.DefaultSkin(defSkin) where

import SDL.Vect
import GUI

formBkColor :: GuiColor; formBkColor =  grayColor 230
disabledColor :: GuiColor; disabledColor = grayColor 128

defSkin :: Skin
defSkin = Skin  {
    skinName = "default"
    , windowBkColor = grayColor 255
    , bkColor = formBkColor
    , borderColor = grayColor 0
    , widgetBorderColor = grayColor 0x60
    , foregroundColor = grayColor 0
    , selectedDecore = DecoreState { decoreBkColor = rgb 0 64 255
                                   , decoreFgColor = grayColor 255
                                   }
    , brdr3DLightColor = grayColor 240
    , brdr3DDarkColor = grayColor 180
    , formItemsMargin = WidgetMarginEvenly 5
    , formTextLineSpacing = 0.75
    , formItemsButtons =
        ButtonDecore { btnDecoreOut =
                            DecoreState { decoreBkColor = formBkColor
                                        , decoreFgColor = grayColor 0
                                        }
                     , btnDecoreFocused =
                            DecoreState { decoreBkColor = rgb 200 200 255
                                        , decoreFgColor = grayColor 0
                                        }
                     , btnDecoreIn  =
                            DecoreState { decoreBkColor = rgb 220 220 255
                                        , decoreFgColor = grayColor 0
                                        }
                     , btnDecorePressed =
                            DecoreState { decoreBkColor = rgb 240 240 255
                                        , decoreFgColor = rgb 0 0 127
                                        }
                     , btnDecoreDisabled =
                            DecoreState { decoreBkColor = formBkColor
                                        , decoreFgColor = disabledColor
                                        }
                     , btnDecoreBorder = BtnBorderRound (grayColor 64)
                     }
    , buttonsSpacing = 10
    , disableFgColor = disabledColor
    , scrollBarWidth = 15
    , scrollBarColor  = rgb 0 255 255
    , arrowBtns =
        ButtonDecore { btnDecoreOut =
                             DecoreState { decoreBkColor = formBkColor
                                         , decoreFgColor = grayColor 0
                                         }
                     , btnDecoreFocused = -- unused
                             DecoreState { decoreBkColor = formBkColor
                                         , decoreFgColor = grayColor 0
                                         }
                     , btnDecoreIn  =
                             DecoreState { decoreBkColor = rgb 230 230 255
                                         , decoreFgColor = grayColor 0
                                         }
                     , btnDecorePressed =
                             DecoreState { decoreBkColor = grayColor 255
                                         , decoreFgColor = grayColor 0
                                         }
                     , btnDecoreDisabled =
                             DecoreState { decoreBkColor = formBkColor
                                         , decoreFgColor = disabledColor
                                       --, decoreBrdrColor = grayColor 0
                                         }
                     , btnDecoreBorder = BtnBorderRound (grayColor 64)
                     }
    , scrollBarSlider =
        ButtonDecore { btnDecoreOut =
                            DecoreState { decoreBkColor = rgb 255 255 0 --   grayColor 220
                                        , decoreFgColor = grayColor 220
                                        }
                     , btnDecoreFocused = -- unused
                            DecoreState { decoreBkColor = rgb 255 255 0 --   grayColor 220
                                        , decoreFgColor = grayColor 220
                                        }
                     , btnDecoreIn  =
                            DecoreState { decoreBkColor = rgb 220 220 255
                                        , decoreFgColor = grayColor 255
                                        }
                     , btnDecorePressed =
                            DecoreState { decoreBkColor = grayColor 250
                                        , decoreFgColor = grayColor 0
                                        }
                     , btnDecoreDisabled =
                            DecoreState { decoreBkColor = grayColor 255
                                        , decoreFgColor = grayColor 255
                                        }
                     , btnDecoreBorder = BtnBorder3D (grayColor 250) (grayColor 180)
                     }
    , scrollBarArrow =
        ButtonDecore { btnDecoreOut =
                            DecoreState { decoreBkColor = grayColor 240
                                        , decoreFgColor = grayColor 160
                                        }
                     , btnDecoreFocused = -- unused
                            DecoreState { decoreBkColor = grayColor 240
                                        , decoreFgColor = grayColor 160
                                        }
                     , btnDecoreIn  =
                            DecoreState { decoreBkColor = rgb 240 240 255
                                        , decoreFgColor = grayColor 0
                                        }
                     , btnDecorePressed =
                            DecoreState { decoreBkColor = grayColor  128
                                        , decoreFgColor = grayColor 0
                                        }
                     , btnDecoreDisabled =
                            DecoreState { decoreBkColor = grayColor 240
                                        , decoreFgColor = grayColor 255
                                        }
                     , btnDecoreBorder = BtnBorder3D (grayColor 255) (grayColor 200)
                     }
    , scrollAreaSlidersColor = V4 0 0 0 40
    , scrollAreaArrowsColor = grayColor 0
    , splitterActive = DecoreState { decoreBkColor = grayColor 0
                                   , decoreFgColor = grayColor 255
                                   }
    , popupMnuBkColor  = rgb 128 0 64
    , popupMnuInColor  = rgb 64 0 128
    , popupMnuFgColor  = rgb 255 255 255
    , popupMnuHotKeyColor  = rgb 0 255 0
    , popupMnuSeparatorColor  = rgb 255 255 0
    , popupMnu3DLightColor  = rgb 255 255 0
    , popupMnu3DDarkColor  = rgb 64 0 32
    }
