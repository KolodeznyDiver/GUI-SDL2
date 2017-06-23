{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module:      GUI.BaseLayer.Depend1.Skin
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <kolodeznydiver@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- GUI Skin и сопутствующие типы.
--
-- Оформление интерфейса задаётся структурой 'Skin', а так же картинками, загружаемыми из поддиректории
-- с именем /ИмяСкина.skin/ находящимся в директории ресурсов, где /ИмяСкина/ берётся из поля @skinName@
-- скина.
--
-- По мере развития пакета GUI-SDL2 в тип Skin, возможно, будут добавляться поля.
-- Тем не менее, в дальнейшем, предполагается его сериализация из (в?) текстовый файл.

module GUI.BaseLayer.Depend1.Skin where

import GHC.Generics (Generic)
import Data.Data (Data)
import Data.Typeable
import GUI.BaseLayer.Depend0.Types

-- | Цвета "3D"-рамки.
data Border3DColors = Border3DColors {
    brdr3DLightColor :: GuiColor -- ^ Цвет левого и верхнего края.
  , brdr3DDarkColor :: GuiColor -- ^ Цвет правого и нижнего края.
  }
  deriving (Show, Read, Data, Eq, Ord, Generic, Typeable)

-- | Описание рамок оформления элементов интерфейса .
data BtnBorderType =
        -- | Рамка в виде прямоугольника из тонкой линии заданного цвета, с закруглёнными краями.
        BtnBorderRound GuiColor
        -- | "3D"-рамка.
      | BtnBorder3D Border3DColors
      deriving (Show, Read, Data, Eq, Ord, Generic, Typeable)

-- | Оформление чего угодно, что имеет цвет фона и цвет переднего плана
data DecoreState = DecoreState  { decoreBkColor   :: GuiColor
                                , decoreFgColor   :: GuiColor
                                }
                    deriving (Show, Read, Data, Eq, Ord, Generic, Typeable)

-- | Описание оформления кнопки, или другого элемента интерфейса имеющего перечисленные ниже состояния
-- в которых оно отображается разным сочетанием цветов.
data ButtonDecore = ButtonDecore {
    btnDecoreOut :: DecoreState -- ^ Цвета элемента когда курсор не над ним.
  , btnDecoreIn  :: DecoreState -- ^ Цвета элемента когда курсор над ним.
  , btnDecoreFocused  :: DecoreState -- ^ Цвета элемента когда элемент имет фокус.
  , btnDecorePressed :: DecoreState -- ^ Цвета элемента когда он нажат.
  , btnDecoreDisabled :: DecoreState -- ^ Цвета элемента когда он disabled.
  , btnDecoreBorder :: BtnBorderType -- ^ Тип и цвета рамки.
                                 }
                    deriving (Show, Read, Data, Eq, Ord, Generic, Typeable)

-- | Собственно, описание оформления GUI.
data Skin = Skin {
    skinName :: String -- ^ Имя скина.
  , windowBkColor :: GuiColor -- ^ Цвет фона основоного окна (не окон форм, диалогов) и окон ввода
                              -- текста.
  , windowFgColor :: GuiColor -- ^ Цвет переднего плана основоного окна (не окон форм, диалогов)
                              -- и окон ввода текста.
  , windowDisabledColor :: GuiColor -- ^ Цвет переднего плана основоного окна (не окон форм, диалогов)
                                    -- и окон ввода текста в состоянии __disabled__.
  , bkColor :: GuiColor -- ^ Цвет фона форм, диалоговых окон.
  , borderColor :: GuiColor -- ^ Цвет рамок элементов на форме
  , foregroundColor :: GuiColor -- ^ Цвет текста и других элементов переднего плана формы, диалога.
  , selectedDecore :: DecoreState -- ^ Цвета выделенной области (например, выделенного фрагмента текста).
  , brdr3DColors :: Border3DColors -- ^ Цвета "3D"-рамок, которые не изменяют цвет в зависимости от состояния.
  , formItemsMargin  :: WidgetMargin -- ^ Поля рамок элементов формы по умолчанию
  , formTextLineSpacing :: Double -- ^ Междустрочное расстояние для текстов элементов формы.
  , formItemsButtons :: ButtonDecore -- ^ Оформление кнопок
  , disabledFgColor  :: GuiColor -- ^ Цвет Цвет переднего плана (текста) элементов формы
                                 -- находящихся в состоянии __disabled__.
  , scrollBarWidth  :: Coord -- ^ Ширины скроллбаров.
  , arrowBtns  :: ButtonDecore -- ^ Оформление кнопок со стрелками (не в составе scrollbar-а)
                               -- пока не используется.
  , scrollBarColor  :: GuiColor -- ^ Цвет полосы скроллбара
  , scrollBarSlider :: ButtonDecore -- ^ Оформление слайдера скроллбара.
                                    -- В настоящий момент цвета переднего плана из 'ButtonDecore'
                                    -- не используются - на слайдере ничего не нарисовано, только
                                    -- рамка и фон.
  , scrollBarArrow :: ButtonDecore -- ^ Оформление кнопок со стрелками в составе scrollbar-а.
  , scrollAreaSlidersColor :: GuiColor -- ^ Цвет слайдеров в скроллируемой области
                                       -- @GUI.Widget.Container.ScrollArea.scrollArea@
  , scrollAreaArrowsColor :: GuiColor -- ^ Цвет кнопок-стрелок в @GUI.Widget.Container.ScrollArea.scrollArea@.
  , splitterActive :: DecoreState -- ^ Цвета активного splitter-а (при его перемещении, когда
                                  -- кнопка мыши нажата.
  , popupMnuBkColor  :: GuiColor -- ^ Цвет фона выпадающего меню.
  , popupMnuInColor  :: GuiColor -- ^ Цвет фона элемента под указателем мыши выпадающего меню.
  , popupMnuFgColor  :: GuiColor -- ^ Цвет переднего плана выпадающего меню.
  , popupMnuHotKeyColor  :: GuiColor -- ^ Цвет текстового обозначения горячих клавиш на выпадающем меню.
  , popupMnuSeparatorColor  :: GuiColor -- ^ Цвет сепаратора (горизонтальной линии - разделителя)
                                        --  выпадающего меню.
  , popupMnuDisabledColor  :: GuiColor -- ^ Цвет переднего плана недоступного пункта выпадающего меню.
  , popupMnuBorderColors :: Border3DColors -- ^ Цвета рамки выпадающего меню.
  }
  deriving (Show, Read, Data, Eq, Ord, Generic, Typeable)
