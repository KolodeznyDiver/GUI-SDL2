{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
-- |
-- Module:      Main
-- Copyright:   (c) 2017-2020 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Набор примеров в одном файле.
--
-- Перед просмотром примеров потребуется или скопировать каталог GUI.Resources в
--    Windows : C:\Users\User\AppData\Roaming\GUIDemo
--    *nix    : ~/.local/share/GUIDemo
-- либо задать переменную окружения GUIDEMO_GUIRESOURCES с полным путём к каталогу
-- GUI.Resources в пакете включая в конце пути и сам GUI.Resources.
--
-- Для просмотра примеров изменять номер
--
-- > #define EXAMPLE_NUM
--
-- в этом файле, пересобрать пример, и посмотреть что получилось:
--
-- > stack build --flag GUI-SDL2:examples --exec GUIDemo
--


#ifndef EXAMPLE_NUM
#define EXAMPLE_NUM 0
#endif

module Main where

import Data.Monoid
import Control.Monad
import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import qualified Data.Text as T
import qualified TextShow as TS
import TextShow (showb,showt)
import qualified Data.Vector as V
import qualified Data.Vector.Utils as V
import Data.Bits
import Data.Maybe
import System.Directory
import Data.Default
import GHC.Conc
import qualified SDL
import SDL.Vect
import qualified SDL.Font as FNT
import GUI
import GUI.Skin.DefaultSkin
import GUI.Widget.Handlers
import GUI.Widget.Label
import GUI.Widget.Layout.LinearLayout
import GUI.Widget.Button
import GUI.Widget.LinearTrackBar
import GUI.Widget.Container.ScrollArea
import GUI.Widget.Splitter
import GUI.Widget.Menu.Horizontal
import GUI.Widget.Container.Border
import GUI.Widget.EditBox
import GUI.Window.MessageBox
import GUI.Widget.ListView
import GUI.Widget.DropDownList
import GUI.Widget.Header
import GUI.Window.LoadSaveDialog
-- import GUI.Widget.HorizontalLinks
-- import GUI.Widget.PathBox
import GUI.Widget.HorizontalTabbeds
import GUI.Widget.HorizontalItems (NeighborSwap(..))
import GUI.Widget.Container.TabbedPanel
import GUI.Widget.CheckBox
import GUI.Widget.RadioButton

main :: IO ()
main = runGUI defSkin  -- Запуск GUI с оформлением по умолчанию

        -- Список предзагруженных шрифтов : ключ, имя файла, размер шрифта, опции
        [GuiFontDef ""            "PTM55F.ttf" 14 def -- по молч., если не найден указанный ключ
        ,GuiFontDef "label"       "PTN57F.ttf" 15 def -- label
        ,GuiFontDef "edit"        "PTN57F.ttf" 14 def{-- fontHinting = Just FNT.None
                                                     , fontKerning = Just False
--                                                     , fontOutline = Just 0
                                                     -}
        ,GuiFontDef "link"        "PTN57F.ttf" 14 def{fontStyle = Just [FNT.Underline]}
        ,GuiFontDef "separator"   "PTN57F.ttf" 14 def{fontStyle = Just [FNT.Bold]}
        ,GuiFontDef "list"        "PTN57F.ttf" 13 def -- listView
        ,GuiFontDef "small"       "PTN57F.ttf" 13 def
        ,GuiFontDef "menu"        "PTN57F.ttf" 14 def
        ,GuiFontDef "hello world" "PTN57F.ttf" 28 def{fontStyle = Just [FNT.Bold, FNT.Italic, FNT.Underline]}
        ]

                    -- Windows : C:\Users\User\AppData\Roaming\GUIDemo\GUIDemo.log
                    -- *nix    : ~/.local/share/GUIDemo/GUIDemo.log
        def{guiLogDef = def{logFileName = "GUIDemo.log"}}
        $ \gui -> do

    (v0,v1,v2) <- SDL.version
    logPutLn gui $ "SDL version " <> showb v0 <> "." <> showb v1 <> "." <> showb (v2 :: Int)

    win <- newWindow gui "GUI test" $ SDL.defaultWindow { SDL.windowInitialSize = V2 400 400
                                                        --, SDL.windowResizable = True
                                                        }
#if EXAMPLE_NUM == 0
    void $ win $+ label def { labelFormItemDef = def{formItemMargin=Just WidgetMarginNone}
                            , labelAlignment=AlignCenter, labelFontKey = "hello world"
                            , labelColor = Just $ rgb 0 100 0
                            , labelText="Привет, мир!"}
#elif EXAMPLE_NUM == 1
    vL <- win $+ vLayout def{layoutAlignment = AlignCenterTop}
    void $ vL $+ label def{labelSize=V2 150 20, labelAlignment=AlignCenter,
                          labelText="Текст вверху, по центру"}

    hL0 <- vL $+ hLayout def
    void $ hL0 $+ label def{labelSize=V2 150 (-1),labelText="Текст слева"}
    btn0 <- hL0 $+ button def{btnSize = V2 100 35, btnText = "Button 1"}

    lb1 <- vL $+ label def{labelSize=V2 250 80, labelWrapMode= TextWrap 0 Nothing, labelAlignment=AlignCenter,
        labelText=
        "Пример текста, который может переходить на другие строки, таким образом образуя многострочный текст."}
    hL1 <- vL $+ hLayout def
    btn1 <- hL1 $+ button def{btnSize = V2 150 150,  btnPicture = ButtonLeftPicture "sync.png"
                , btnTextWrapMode = TextWrap 0 Nothing
                , btnText = "Сделать правую кнопку недоступной"
                }
    btn2 <- hL1 $+ button def{btnSize = V2 100 150,  btnPicture = ButtonBottomPicture "check.png"
                , btnTextWrapMode = TextWrap 0 Nothing
                , btnText = "Сделать правую кнопку снова доступной"  }
    btn3 <- hL1 $+ button def{btnSize = V2 100 150,  btnPicture = ButtonBottomPicture "rss_circle.png"
                , btnTextWrapMode = TextWrap 0 Nothing
                , btnText = "Эту кнопку сделаем недоступной после нажатия левой"  }

    -- Реакции на события

    onClick btn0 $ setText lb1
      "После нажатия на кнопки они становятся Focused и их можно перебирать по Tab/Shift-Tab, щёлкать Enter или пробел"
    onClick btn1 $  -- Переводим 3-ю кнопку в disable
        enableWidget btn3 False
    onClick btn2 $  -- Переводим 3-ю кнопку в enable
        enableWidget btn3 True
    onClick btn3 $
        SDL.showSimpleMessageBox Nothing SDL.Information "Заголовок сообщения" "Правая кнопка была нажата"
#elif EXAMPLE_NUM == 2
    vL <- win $+ vLayout def{layoutAlignment = AlignCenterTop}
    void $ vL $+ label def{labelSize=V2 300 20, labelAlignment=AlignCenter,
                          labelText="Это трекбар которым можно задавать значения"}
    hL0 <- vL $+ hLayout def
    lbValue <- hL0 $+ label def{labelSize=V2 70 20, labelAlignment=AlignLeftCenter,
                          labelText="<значение>"}

    trBar <- hL0 $+ hTrackBar def{
                               linearTrackBarLn = 200
                             , linearTrackBarFormItemDef = def{formItemMargin= Just $ WidgetMarginXY 10 0}
                             , linearTrackMinValue = 0
                             , linearTrackMaxValue = 1000
                             , linearTrackBarPos  = 300
                                          }
    -- при перетаскивании scrollBar-а соответствующее значение отображается в тексте метки
    onChanged trBar $ \ v -> setText lbValue (T.pack $ show (v:: Int))

    hL1 <- vL $+ hLayout def
    btn1 <- hL1 $+ button def{btnSize = V2 100 60, btnTextWrapMode = TextWrap 0 Nothing
                , btnText = "Установить в минимум"  }
    onClick btn1 $ setValue trBar (0::Int)
    btn2 <- hL1 $+ button def{btnSize = V2 100 60, btnTextWrapMode = TextWrap 0 Nothing
                , btnText = "Установить среднее значение"  }
    onClick btn2 $ setValue trBar (500::Int)
    btn3 <- hL1 $+ button def{btnSize = V2 100 60, btnTextWrapMode = TextWrap 0 Nothing
                , btnText = "Установить в максимум"  }
    onClick btn3 $ setValue trBar (1000::Int)
    void $ vL $+ label def{labelSize=V2 300 20, labelAlignment=AlignCenter,
                          labelText="Ниже кнопки с треугольниками, полезные в интерфейсе"}
    hL2 <- vL $+ hLayout def


    let btTriangleDef = ButtonWithTriangleDef   { btTriangleFormItemDef =
                                                    def{formItemMargin= Just $ WidgetMarginXY 3 0}
                                                , btTriangleFlags = WidgetVisible .|. WidgetEnable
                                                , btTriangleOrientation = OrientationLeft
                                                , btTriangleSize = V2 14 14
                                                , btTriangleType = ButtonWithTriangleInForm
                                                }
    btnL <- hL2 $+ buttonWithTriangle btTriangleDef
    btnR <- hL2 $+ buttonWithTriangle btTriangleDef{btTriangleOrientation = OrientationRight}
    btnU <- hL2 $+ buttonWithTriangle btTriangleDef{btTriangleOrientation = OrientationUp}
    btnD <- hL2 $+ buttonWithTriangle btTriangleDef{btTriangleOrientation = OrientationDown}

    void $ vL $+ label def{labelSize=V2 300 20, labelAlignment=AlignCenter,
                          labelText="В примере они меняют цвет значения"}
    onClick btnL $ setTextColor lbValue $ rgb 255 0 0
    onClick btnR $ setTextColor lbValue $ rgb 0 255 0
    onClick btnU $ setTextColor lbValue $ rgb 0 0 255
    onClick btnD $ setTextColor lbValue $ rgb 0 0 0
#elif EXAMPLE_NUM == 3
    vL <- win $+ vLayout def{layoutAlignment = AlignCenterTop}
    void $ vL $+ label def{labelSize=V2 350 60, labelAlignment=AlignCenter, labelWrapMode= TextWrap 0 Nothing,
                          labelText=
      "Область с прозрачными линейками прокрутки. Кнопка внизу справа с особенностью. Для меня фишка, а кому то не по нравится"
                          }
    hL0 <- vL $+ hLayout def
    void $ hL0 $+ label def{labelSize=V2 60 (-1), labelAlignment=AlignLeftCenter
                            , labelWrapMode= TextWrap 0 Nothing
                            ,labelFormItemDef = def{formItemMargin= Just $ WidgetMarginLTRB 10 0 0 0}
                            ,labelText=
       "Кнопка внизу справа получает разное назна- чение в зависи- мости от направле- ния захода в неё мышью"
                           }
    sa <- hL0 $+ scrollArea def
    void $ sa $+ exampleTextGrid

#elif EXAMPLE_NUM == 4
    vL <- win $+ vLayout def{layoutAlignment = AlignCenterTop}
    void $ vL $+ label def{labelSize=V2 300 60, labelAlignment=AlignCenter, labelWrapMode= TextWrap 0 Nothing,
                           labelText=
        "Между зелёным и жёлтым прямоугольниками есть splitter. Наведите туда мышь и потаксайте влево - вправо"}
    hL0 <- vL $+ hLayout def{layoutAlignment = AlignLeftCenter}
    void $ hL0 $+ colorWidget (rgb 0 0 255 )
    void $ hL0 $+ splitter
    void $ hL0 $+ colorWidget (rgb 255 255 0)
#elif EXAMPLE_NUM == 5
    vL <- win $+ vLayout def{layoutAlignment = AlignCenterTop}
    void $ vL $+ label def{labelSize=V2 450 20, labelAlignment=AlignCenter,
                              labelText="Пример создания виджета отслеживающего мышь"}
    void $ vL $+ mouseChkWidget $ WidgetMarginXY 20 10
#elif EXAMPLE_NUM == 6
    lb <- win $+ label def{labelAlignment=AlignCenter
                           , labelFormItemDef= def{formItemMargin= Just WidgetMarginNone}
                           , labelText= "Через 2 с здесь будут данные из трэда"}

    pipe <- newGuiPipe gui $ \ _ v ->
        setText lb $ TS.toText $ "Разрешение дисплея " <> showb (v V.! 0) <> "x" <> showb (v V.! 1)

    void $ forkIO $ do
        threadDelay 2000000
        (V2 xRes yRes) <- (SDL.displayBoundsSize . head) <$> SDL.getDisplays
        void $ sendToGuiPipe pipe $ V.fromList [xRes,yRes]
#elif EXAMPLE_NUM == 7
    vL <- win $+ vLayout def{layoutAlignment = AlignCenterTop}

    -- Define actions
    addActions gui "File" [
         ("New",def{actionText="Новый", actionHotKey=kCtrl SDL.KeycodeN, actionPicture="new.ico"})
        ,("Open",def{actionText="Открыть", actionHotKey=kCtrl SDL.KeycodeO, actionPicture="open.png"})
        ,("Save",def{actionText="Сохранить", actionHotKey=kCtrl SDL.KeycodeS, actionPicture="save.png"})
        ,("SaveAs",def{actionText="Сохранить как", actionPicture="saveas.png"})]

    addActions gui "Edit" [
         ("Cut",def{actionText="Вырезать", actionHotKey=kCtrl SDL.KeycodeX, actionPicture="cut.png"})
        ,("Copy",def{actionText="Копировать", actionHotKey=kCtrl SDL.KeycodeC, actionPicture="copy.png"})
        ,("Paste",def{actionText="Вставить", actionHotKey=kCtrl SDL.KeycodeV, actionPicture="paste.png"})]

    addActions gui "Application" [
         ("Exit",def{actionText="Выход", actionHotKey=kAlt SDL.KeycodeF4, actionPicture="exit.png"
            , actionValue = def{onAction=guiApplicationExitSuccess gui}})]

    addActions gui "Find" [
         ("Find",def{actionText="Поиск"})
        ,("Replace",def{actionText="Замена"})
         ]

    addActions gui "FileFind" [
         ("Find",def{actionText="Поиск в файлах"})
        ,("Replace",def{actionText="Замена в файлах"})
         ]

    addActions gui "doSubmenu" [
         ("SomeSubmenu",def{actionText="Некое подменю"})
         ]

    addActions gui "ExamplSub" [
         ("Item 1",def{actionText="Действие 1"})
        ,("Item 2",def{actionText="Действие 2"})
        ,("Item 3",def{actionText="Действие 3"})
         ]

    -- Define popup menus
    let popupFile = mkMenu (mItem "File" "New") (mItem "File" "Open") (mItem "File" "Save") (mItem "File" "SaveAs")
                           (mItemSub "doSubmenu" "SomeSubmenu" $ mkMenu (mItem "ExamplSub" "Item 1")
                                                                        (mItem "ExamplSub" "Item 2")
                                                                        (mItem "ExamplSub" "Item 3"))
                           (mItem    "Application" "Exit")
        popupEdit = mkMenu (mItem "Edit" "Cut") (mItem "Edit" "Copy") (mItem "Edit" "Paste")
        popupFind = mkMenu (mItem "Find" "Find") (mItem "Find" "Replace")
                           (mItem "FileFind" "Find") (mItem "FileFind" "Replace")

    void $ vL $+ horizontalMenu def{hmenuItems = mkHMenu
        def{hmenuText = "Файл", hmenuPopup = popupFile}
        def{hmenuText = "Правка", hmenuPopup = popupEdit}
        def{hmenuText = "Поиск", hmenuPopup = popupFind}
                                    }

    void $ vL $+ border def { borderFormItemDef = def{formItemMargin= Just WidgetMarginNone}
                            , borderSize = V2 (-1) 2, borderType = BorderMono
                            , borderBkgrnd = BorderBkColor $ grayColor 255
                            }

    let txtHotKeyPrompt = "Попробуйте нажать Alt-F12 или Ctrl-D"
    lb <- vL $+ label def{labelSize=V2 (-1) 20, labelAlignment=AlignCenter, labelText=txtHotKeyPrompt}

    hL0 <- vL $+ hLayout def
    btn0 <- hL0 $+ button def{btnSize = V2 200 35, btnText = "Восстановить текст"}
    onClick btn0 $
        setText lb txtHotKeyPrompt
        
    addActions gui "Hotkeys" [
         ("hk0",def{actionHotKey= kAlt SDL.KeycodeF12, actionValue=def{onAction= setText lb "Alt-F12"}})
        ,("hk1",def{actionHotKey= kCtrl SDL.KeycodeD, actionValue=def{onAction= setText lb "Ctrl-D"}})
        ,("hkExceptionCatchTest",def{actionHotKey= kShift SDL.KeycodeF1, actionValue=def{onAction=
            setText lb $ showt $ 1 `div` (0 :: Int)
            }})
        ]

    -- Пример задания/замены действия для Action после его создания.
    setAction gui "File" "Save" $ setText lb "Нажат пункт меню File/Save"
    setAction gui "File" "New" $ messageBox gui MsgBoxRetrySkipCancel
        "Это модальное окно, когда оно активно, невозможно выбрать ранее созданные окна - фокус возвращается к модальному окну"
         $ \case
                ButtonRetry -> say gui MsgBoxInfo "Была нажата ButtonRetry"
                ButtonSkip -> say gui MsgBoxWarning "Была нажата ButtonSkip"
                ButtonCancel -> say gui MsgBoxOk "Была нажата ButtonCancel"
                k -> say gui MsgBoxError $ "Завершение с кодом " <> showb (fromEnum k)
    setAction gui "File" "Open" $ textInput gui "Наберите чего ни будь"
        def{
              textInputPrompt = "Вводить сюда :"
            , textInputAccept = \case
                                    Just t -> setText lb t
                                    _ -> setText lb "<Отменено>"
           }
#elif EXAMPLE_NUM == 8
    vL <- win $+ vLayout def{layoutAlignment = AlignCenterTop}
    hL0 <- vL $+ hLayout def
    void $ hL0 $+ border def {borderSize = V2 150 100, borderType = BorderRound
                             , borderThickness = 20, borderCaption = "border 1"
                             }
    void $ hL0 $+ border def {borderSize = V2 150 100, borderType = BorderDot 4
                             , borderThickness = 20, borderCaptionAlignment = HCenter
                             , borderCaption = "border 2"}
    void $ vL $+ border def  {borderSize = V2 (-1) 20, borderType = BorderLine
                             , borderCaption = "border 3"}
    hL1 <- vL $+ hLayout def
    brd4 <- hL1 $+ border def {borderSize = V2 150 100, borderType = Border3D Nothing Nothing
                              , borderThickness = 20, borderCaption = "border 4"}
    void $ brd4 $+ exampleTextGrid

    void $ hL1 $+ border def {borderSize = V2 150 100, borderType = BorderMono
                             , borderFgColor = Just (rgb 255 0 255), borderCaption = "border 5"
                             , borderBkgrnd = BorderBkColor $ rgb 0 255 0}

    fgBorder <- newForeground win (P (V2 130 90)) $
                 border def {  borderType = BorderRect
                             , borderThickness = 1, borderSizeByChild = True
                             , borderBkgrnd = BorderBkColor $ rgb 255 255 200
                             }
    void $ fgBorder $+ label def{labelAlignment=AlignCenter, labelBkColor = Just (rgb 255 255 200)
                             , labelSize = V2 50 25, labelWrapMode = TextNoWrap 300
--                           , labelFormItemDef= def{formItemMargin= $ Just WidgetMarginNone}
                           , labelText= "Пример плавающего виджета"}

#elif EXAMPLE_NUM == 9
    void $ win $+ mouseChkWidget WidgetMarginNone
    fgBorder <- newForeground win (P (V2 130 90)) $
                 border def {  borderType = BorderRect
                             , borderThickness = 1, borderSizeByChild = True
                             , borderBkgrnd = BorderBkColor $ rgb 255 255 200
                             }
    btn0 <- fgBorder $+ button def{btnSize = V2 200 35, btnText = "Плавающая кнопка"}
    onClick btn0 $
        SDL.showSimpleMessageBox Nothing SDL.Information "GUI Demo" "Плавающая кнопка была нажата"
#elif EXAMPLE_NUM == 10
    vL <- win $+ vLayout def -- {layoutAlignment = AlignCenterTop}
    hL0 <- vL $+ hLayout def
    lb <- hL0 $+ label def{labelSize=V2 150 20, labelText="Поле редактирования :"}
    ed <- hL0 $+ editBox def{editBoxText="qwerty012"}
    onChanged ed $ \ t ->
        setText lb t
    onEnd ed $ \ _t ->
        setText lb "Enter or lost focuse"
    btn <- vL $+ button def{btnSize = V2 200 35, btnText = "disable/enable"}
    onClick btn $
        allWidgetFlags (baseWidget ed) WidgetEnable >>= enableWidget ed . not

    setFocus ed
#elif EXAMPLE_NUM == 11
    let textVector = V.generate 20 $ \i -> TS.toText $ "Элемент номер " <> showb i
    vL <- win $+ vLayout def
    lb <- vL $+ label def{labelSize=V2 350 20, labelText="Здесь будет отображаться текущая строка"}
    lstView <- vL $+ listView def{listViewSize = V2 350 200
                                 ,listViewListViewFlags = listViewListViewFlags def  .|. MultiSelectListViewFlag
                                 } $ ListViewText textVector
    onMove lstView $ \ i ->
        setText lb $ textVector V.! i
    onDoubleClick1 lstView $ \i -> do
--        i <- getIx lstView
        setText lb $ TS.toText $ "Двойной щелчёк на элементе номер " <> showb i
    btn <- vL $+ button def{btnSize = V2 200 35, btnText = "disable/enable"}
    onClick btn $
        allWidgetFlags (baseWidget lstView) WidgetEnable >>= enableWidget lstView . not

    setFocus lstView
#elif EXAMPLE_NUM == 12
    let textVector = V.generate 20 $ \i -> TS.toText $ "Элемент номер " <> showb i
    vL <- win $+ vLayout def
    lb <- vL $+ label def{labelSize=V2 350 20, labelText="Здесь будет отображаться текущая строка"}
    ddL <- vL $+ dropDownList def{ ddListSize = V2 300 0 -- 0 = высота не меньше 0, т.е. автоопределение высоты.
                                 } $ ListViewText textVector
    onChanged ddL $ \i ->
        setText lb $ textVector V.! i
    btn <- vL $+ button def{btnSize = V2 200 35, btnText = "disable/enable"}
    onClick btn $
        allWidgetFlags (baseWidget ddL) WidgetEnable >>= enableWidget ddL . not
#elif EXAMPLE_NUM == 13
    vL <- win $+ vLayout def
    lb <- vL $+ label def{labelSize=V2 350 20, labelText="Здесь будет отображаться информация о событиях"}
    hdr <- vL $+ header def{ headerColumns = V.fromList [("Имя файла",150),("Расширение",60),
                                                         ("Размер файла",90)]
                           , headerSortMode = Just (0,Ascending)
                           }
    onWidthsChange hdr $ \widths ->
        setText lb $ TS.showtList $ V.toList widths
    onSortChange hdr $ \col sm ->
        setText lb $ TS.toText $ "col=" <> showb col <> ", " <> TS.fromString (show sm)
#elif EXAMPLE_NUM == 14
    vL <- win $+ vLayout def
    lb <- vL $+ label def{labelSize=V2 350 (-1) , labelWrapMode = TextWrap 0 Nothing
                 , labelText="Здесь будет отображаться информация о событиях"}
#if 0
    -- Проверка вспомогательного виджета, косвенно используемоего в loadSaveDialog
    hLnks <- vL $+ horizLinks def{ horizLnFirstSep = "["
                                 , horizLnSep = "/"
                                 , horizLnLastSep = "]"
                                 , horizLnOptBtn = "plus.png"
                                 , horizLnFlags = WidgetVisible .|. WidgetEnable .|. WidgetFocusable
                                 }
    setFocus hLnks
    onClick1 hLnks $ \i -> setText lb $ TS.toText $ showb i
    setValue hLnks $ V.fromList $ T.words "Съешь же ещё этих мягких французских булок, да выпей чаю."
#else
    let doDlg :: MonadIO m => m ()
        doDlg = loadSaveDialog gui SaveDialog "" def $ \_state v ->
                    (setText lb $ T.intercalate "\n" $ map T.pack $ V.toList v)
    btn <- vL $+ button def{btnSize = V2 300 35, btnText = "Повторить вызов диалога"}
    onClick btn doDlg
    doDlg
#endif
#elif EXAMPLE_NUM == 15
    vL <- win $+ vLayout def
    lb <- vL $+ label def{labelSize=V2 350 (-1) , labelWrapMode = TextWrap 0 Nothing
                 , labelText="Здесь будет отображаться информация о событиях"}
    ht <- vL $+ horizTab def{
            hTabCanDelete = True, -- ^ Добавить кнопку с крестиком. Само удаление реализуется вне  @horizTab@
            hTabPermutable = True -- ^ Разрешить перетаскивать закладки мышью.
                            }
    setValue ht $ V.fromList [
          TabItem "Съешь" (rgb 0 0 0)
        , TabItem "же" (rgb 255 0 0 )
        , TabItem "ещё" (rgb  0 255 0)
        , TabItem "этих" (rgb  128 128 0)
        , TabItem "мягких" (rgb  0 0 255)
        , TabItem "французских" (rgb  128 0 128)
        , TabItem "булок," (rgb  0 128 128)
        , TabItem "да" (rgb  0 0 0)
        , TabItem "выпей" (rgb  128 128 0)
        , TabItem "чаю." (rgb  128 0 0)
                             ]
    onMove ht (setText lb . TS.toText . showb)
    --  Поддержка перестановки элементов мышью.
    setNeighborSwap ht $ \i v -> return $ V.swapNeighb i v
#elif EXAMPLE_NUM == 16
    vL <- win $+ vLayout def
    lb <- vL $+ label def{ labelSize=V2 350 50
                         , labelText="Здесь будет отображаться информация о событиях"}
    tp <- vL $+ tabbedPanel def{tabPanelPermutable = True}
    let mkTab s = do
            vLTabbed <- tabbedPanelAppend tp def{tabItemCaption=s} $ vLayout def
            void $ vLTabbed $+ label def { labelFormItemDef = def{formItemMargin=Just WidgetMarginNone}
                            , labelAlignment=AlignCenter, labelFontKey = "hello world"
                            , labelColor = Just $ rgb 0 100 0, labelSize=V2 (-1) (-1)
                            , labelText=s}
    mapM_ mkTab $ T.words "Пример панели с закладками которые можно менять местами перетаскивая"
    setIx tp 0
    onMove tp (setText lb . TS.toText . showb)
#elif EXAMPLE_NUM == 17
    hL <- win $+ hLayout def
    brdr <- hL $+ border def  { borderSize = V2 250 (-1), borderType = BorderRect
                              , borderCaption = "checkBox\'s"
                              }
    vbL <- brdr $+ vLayout def  { layoutAlignment= AlignLeftTop }
    cbs <- mapM (\t -> vbL $+ checkBox def{ checkBoxSize = V2 200 20
                                          , checkBoxFormItemDef=def{formItemMargin=Just $ WidgetMarginXY 15 0}
                                          , checkBoxTextWrapMode = TextWrap 100 Nothing
                                          , checkBoxText = t})
            ["Это widget checkBox с именем chkb0","Другой checkBox"
            ,"И ещё один checkBox с самым длинным текстом на три строки"
            ,"Последний checkBox"]
    btn1 <- hL $+ button def{btnSize = V2 100 60, btnTextWrapMode = TextWrap 0 Nothing
                , btnText = "Переключить chkb0"  }
    let chkb0 = head cbs
        chkb1 = cbs !! 1
    onClick btn1 $ getValue chkb0 >>=  setValue chkb0 . not
    onChanged chkb0 $ \state -> enableWidget chkb1 state
#elif EXAMPLE_NUM == 18
    hL <- win $+ hLayout def
    brdr <- hL $+ border def  { borderSize = V2 250 (-1), borderType = BorderRect
                              , borderCaption = "radioButton"
                              }
    rb <- brdr $+ radioButton def   { radioButtonSize = V2 (-1) (-1)
                                    , radioButtonTextWrapMode = TextWrap 100 Nothing
                                    }
                              (boundBased ["что то весьма небольшое, просто не на что смотреть"
                                          ,"то что надо"
                                          ,"ну, это просто огромное" ]) EQ
    vL <- hL $+ vLayout def
    lb <- vL $+ label def{ labelSize=V2 100 50
                         , labelText="- - -"}
    btn <- vL $+ button def{btnSize = V2 100 60, btnTextWrapMode = TextWrap 0 Nothing
               , btnText = "Установить GT"  }
    onClick btn $ setValue rb GT
    onChanged rb (setText lb . TS.toText . showb)
#else
    #error EXAMPLE_NUM is out of range
#endif

----- Поезные заметки
-- Fonts UbuntuMono-*.ttf loaded from
-- http://ru.fonts2u.com/download/ubuntu-mono.%D1%81%D0%B5%D0%BC%D0%B5%D0%B9%D1%81%D1%82%D0%B2%D0%BE
-- Fonts PT*.ttf loaded from
-- http://www.paratype.ru/public/


#if (EXAMPLE_NUM == 3) || (EXAMPLE_NUM == 8)
exampleTextGrid :: MonadIO m => Widget -> Skin -> m (GuiWidget SimpleWidget)
exampleTextGrid parent _ = mkWidget (WidgetVisible .|. WidgetEnable .|. WidgetFocusable)
                                WidgetMarginNone SimpleWidget
                                parent def{
        onCreate = \widget -> setWidgetCanvasRect widget (SDL.Rectangle zero (V2 500 800)) >>
                                 notifyParentAboutSize widget zero
        , onResizing= setWidgetRect
        , onDraw= \widget -> do
            rect@(SDL.Rectangle (P (V2 l t)) (V2 w h)) <- getWidgetCanvasRect widget
            setColor $ grayColor 255
            fillRect rect
            fnt <- getFont ""
            let dx = 70
                dy = 20
                txtColor = V4 0 0 100 0
                txtFill r y =
                    let txtRow c x = when (x < w) $ do
                                drawText fnt txtColor (P (V2 x y)) $ TS.toText $ showb x <> "," <> showb y
                                txtRow (succ c) (x+dx)
                    in when (y < h) $ do
                        txtRow (0::Int) l
                        txtFill (succ r) (y+dy)
            txtFill (0::Int) t
                                }
#endif

#if EXAMPLE_NUM == 4
colorWidget :: MonadIO m => GuiColor -> Widget -> Skin -> m (GuiWidget SimpleWidget)
colorWidget color parent _ = mkSimpleWidget (WidgetMarginXY 20 10) parent $ colorRectFns (V2 60 30) color
#endif

#if (EXAMPLE_NUM == 5) || (EXAMPLE_NUM == 9)
mouseChkWidget :: MonadIO m => WidgetMargin -> Widget -> Skin -> m (GuiWidget SimpleWidget)
mouseChkWidget margin parent _ = do
    rfState <- newMonadIORef Nothing
    mkWidget (WidgetVisible .|. WidgetEnable) margin SimpleWidget parent
                (noChildrenFns $ V2 (-1) (-1)){
        onGainedMouseFocus = \widget p -> writeMonadIORef rfState (Just p) >> markWidgetForRedraw widget
        ,onMouseMotion = \widget _btnsLst p _relMv -> writeMonadIORef rfState (Just p) >> markWidgetForRedraw widget
        ,onLostMouseFocus = \widget -> writeMonadIORef rfState Nothing >> markWidgetForRedraw widget
        ,onDraw= \widget -> do
            visibleRect <- getVisibleRect widget
            setColor $ grayColor 100
            fillRect visibleRect
            fnt <- getFont ""
            state <- readMonadIORef rfState
            s <- case state of
                Just p@(P (V2 x y)) -> do
                    setColor $ grayColor 255
                    let crossSz = 10
                    drawLine (p .-^ V2 crossSz 0) (p .+^ V2 crossSz 0)
                    drawLine (p .-^ V2 0 crossSz) (p .+^ V2 0 crossSz)
                    return $ TS.toText $ showb x <> "x" <> showb y
                _ -> return "No mouse"
            drawText fnt (grayColor 255) (P (V2 5 5)) s
                                                            }
#endif

{- Ниже приведены виджеты не включённые в этот набор примеров. Вы можете их подключить или переделать самостоятельно -}

-- Цветной хеллоуворд
exampleWidgetHelloWorld :: MonadIO m => Widget -> Skin -> m (GuiWidget SimpleWidget)
exampleWidgetHelloWorld parent _ = mkSimpleWidget (WidgetMarginXY 20 10) parent (noChildrenFns $ V2 0 60){
        onDraw= \ widget -> do
                visibleRect <- getVisibleRect widget
                setColor $ V4 0 0 255 0
                fillRect visibleRect
                setColor $ V4 255 0 0 0
                drawRect $ shrinkRect' 5 visibleRect
                fnt <- getFont ""
                drawTextAligned fnt AlignCenter (V4 255 255 255 0) DrawStrFine visibleRect "Привет, мир!"
                        }

-- отображение картинки, возможно с прозрачными областями
exampleWidgetPicture :: MonadIO m => Widget -> Skin -> m (GuiWidget SimpleWidget)
exampleWidgetPicture parent _ = mkSimpleWidget (WidgetMarginXY 20 10) parent (noChildrenFns $ V2 30 140){
        onDraw= \widget -> do
                    visibleRect <- getVisibleRect widget
                    setColor $ V4 255 255 255 0
                    fillRect visibleRect
                    drawTextureR "ArrBtns.png" zero
                    -- drawStretchedTextureR "snapshot_00.41.57.jpg" Nothing visibleRect
                }

-- Эксперименты с прозрачностями линий и текстур
blendModeTest :: MonadIO m => Widget -> Skin -> m (GuiWidget SimpleWidget)
blendModeTest parent _ = mkSimpleWidget (WidgetMarginXY 20 10) parent (noChildrenFns $ V2 300 120){
    onDraw= \widget -> do
            visibleRect <- getVisibleRect widget
            let bkgColor = grayColor 230
            setColor bkgColor
            fillRect visibleRect
            testDraw 10
            withBlendMode SDL.BlendAlphaBlend $ testDraw 40 -- включает прозрачность на линиях по уровню 4-го параметра V4 ( 0 - не прозр)
            -- withBlendMode SDL.BlendAdditive $ testDraw 70 -- линиц нет
            -- withBlendMode SDL.BlendMod $ testDraw 100 -- линии обычные
            fnt <- getFont ""
            tRed <- fromJust <$> renderText fnt (V4 255 0 0 100) "Тест"
            tPic <- getTexture "questmark.bmp"
            drawTexture tRed $ P (V2 70 10)
            drawTexture tPic $ P (V2 110 10)
            withTransparentTexture 100 tRed $ drawTexture tRed $ P (V2 70 30)
            withTransparentTexture 100 tPic $ drawTexture tPic $ P (V2 110 30)
            lift $ SDL.destroyTexture tRed
                            }
  where testDraw y = do
            setColor $ V4 0 0 0 100
            drawRect $ SDL.Rectangle (P (V2 10 y)) (V2 20 20)
            setColor $ V4 255 0 0 100
            drawRect $ SDL.Rectangle (P (V2 40 y)) (V2 20 20)

-- Делаем простенький виджет
framesTest :: MonadIO m => Widget -> Skin -> m (GuiWidget SimpleWidget)
framesTest parent _skin = mkSimpleWidget WidgetMarginNone parent (noChildrenFns $ V2 (-1) (-1)){
            onDraw= \ widget -> do
                visibleRect <- getVisibleRect widget
                setColor $ grayColor 255
                fillRect visibleRect
                let drawR = drawRoundFrame (rgb 255 0 0) (rgb 0 255 0)
                drawR $ SDL.Rectangle (P (V2 10 10)) (V2 33 27)
                drawR $ SDL.Rectangle (P (V2 50 10)) (V2 50 18)
                drawR $ SDL.Rectangle (P (V2 10 50)) (V2 300 27)
                drawR $ SDL.Rectangle (P (V2 10 100)) (V2 333 227)
            }

