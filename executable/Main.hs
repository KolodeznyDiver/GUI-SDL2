{-# LANGUAGE CPP #-}
-- Для просмотра примеров изменять номер и перекомпилировать
#define EXAMPLE_NUM 0
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import qualified Data.Text as T
--import qualified Data.Vector.Storable as V
import qualified Data.Vector as V
import Data.Bits
import Data.Maybe
import Data.Default
import System.Exit
import GHC.Conc
import qualified SDL
import SDL.Vect
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
--import GUI.BaseLayer.PopupWindow

main :: IO ()
main = runGUI defSkin  -- Запуск GUI с оформлением ("кожей", скином) по умолчанию

        -- Таблица предзагруженных шрифтов : ключ, имя файла, размер шрифта
        [GuiFontDef ""          "PTM55F.ttf" 14
        ,GuiFontDef "label"     "PTN57F.ttf" 15
        ,GuiFontDef "small"     "PTN57F.ttf" 13
        ,GuiFontDef "menu"      "PTN57F.ttf" 14]

        $ \gui -> do
    putStr "SDL version " >> SDL.version >>= print
    win <- newWindow gui "GUI test" $ SDL.defaultWindow { SDL.windowInitialSize = V2 400 400
                                                        --, SDL.windowResizable = True
                                                        }
#if EXAMPLE_NUM == 0
    void $ win $+ label def{labelAlignment=AlignCenter, labelText="Привет, мир!"}
#elif EXAMPLE_NUM == 1
    vL <- win $+ vLayout def{layoutAlignment = AlignCenterTop}
    lb0 <- vL $+ label def{labelSize=V2 150 20, labelAlignment=AlignCenter,
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
                             , linearTrackBarMargin = WidgetMarginXY 10 0
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


    let btTriangleDef = ButtonWithTriangleDef   { btTriangleFormItemDef = FormItemWidgetDef $ Just $
                                                                            WidgetMarginXY 3 0
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
                            ,labelFormItemDef = FormItemWidgetDef $ Just $ WidgetMarginLTRB 10 0 0 0
                            ,labelText=
       "Кнопка внизу справа получает разное назна- чение в зависи- мости от направле- ния захода в неё мышью"
                           }
    sa <- hL0 $+ scrollArea def
    void $ sa $+ exampleTextGrid

#elif EXAMPLE_NUM == 4
    vL <- win $+ vLayout def{layoutAlignment = AlignCenterTop}
    void $ vL $+ label def{labelSize=V2 300 60, labelAlignment=AlignCenter, labelWrapMode= TextWrap 0 Nothing,
                           labelText=
        "Между зелёным и жёлтым прямоугольниками есть splitter. Наведите туда мышь и потаксайте лево - вправо"}
    hL0 <- vL $+ hLayout def{layoutAlignment = AlignLeftCenter}
    void $ hL0 $+ colorWidget (rgb 0 0 255 )
    void $ hL0 $+ splitter
    void $ hL0 $+ colorWidget (rgb 255 255 0)
#elif EXAMPLE_NUM == 5
    vL <- win $+ vLayout def{layoutAlignment = AlignCenterTop}
    void $ vL $+ label def{labelSize=V2 450 20, labelAlignment=AlignCenter,
                              labelText="Пример создания виджета отслеживающего мышь"}
    void $ vL $+ mouseChkWidget
#elif EXAMPLE_NUM == 6
    lb <- win $+ label def{labelAlignment=AlignCenter
                           , labelFormItemDef= FormItemWidgetDef $ Just WidgetMarginNone
                           , labelText= "Через 2 с здесь будут данные из трэда"}

    pipe <- newGuiPipe gui $ \ _ v -> do
        let toText ix = T.pack $ show $ v V.! ix
        setText lb $ T.concat ["Разрешение дисплея ", toText 0,"x",toText 1]

    void $ forkIO $ do
        threadDelay 2000000
        (V2 xRes yRes) <- (SDL.displayBoundsSize . head) <$> SDL.getDisplays
        void $ sendToGuiPipe pipe $ V.singleton xRes `V.snoc` yRes
#elif EXAMPLE_NUM == 7
    vL <- win $+ vLayout def{layoutAlignment = AlignCenterTop}

    -- Define actions
    addActions gui "File" [
         ("New",def{actionText="Новый", actionHotKey=hkCtrl SDL.KeycodeN, actionPicture="new.ico"})
        ,("Open",def{actionText="Открыть", actionHotKey=hkCtrl SDL.KeycodeO, actionPicture="open.png"})
        ,("Save",def{actionText="Сохранить", actionHotKey=hkCtrl SDL.KeycodeS, actionPicture="save.png"})
        ,("SaveAs",def{actionText="Сохранить как", actionPicture="saveas.png"})]

    addActions gui "Edit" [
         ("Cut",def{actionText="Вырезать", actionHotKey=hkCtrl SDL.KeycodeX, actionPicture="cut.png"})
        ,("Copy",def{actionText="Копировать", actionHotKey=hkCtrl SDL.KeycodeC, actionPicture="copy.png"})
        ,("Paste",def{actionText="Вставить", actionHotKey=hkCtrl SDL.KeycodeV, actionPicture="paste.png"})]

    addActions gui "Application" [
         ("Exit",def{actionText="Выход", actionHotKey=hkAlt SDL.KeycodeF4, actionPicture="exit.png"
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

    void $ vL $+ border def { borderFormItemDef = FormItemWidgetDef $ Just WidgetMarginNone
                            , borderSize = V2 (-1) 2, borderType = BorderMono
                            , borderBkgrnd = BorderBkColor $ grayColor 255
                            }

    let txtHotKeyPrompt = "Попробуйте нажать Alt-F2 или Ctrl-D"
    lb <- vL $+ label def{labelSize=V2 (-1) 20, labelAlignment=AlignCenter, labelText=txtHotKeyPrompt}

    hL0 <- vL $+ hLayout def
    btn0 <- hL0 $+ button def{btnSize = V2 200 35, btnText = "Восстановить текст"}
    onClick btn0 $ do
        setText lb txtHotKeyPrompt
{-        let widget = getWidget btn0
        sz <- sizeOfRect <$> getWidgetRect widget
        void $ mkPopupWindow widget $ SDL.Rectangle (P sz) (V2 200 400) -}
    addActions gui "Hotkeys" [
         ("hk0",def{actionHotKey= hkAlt SDL.KeycodeF2, actionValue=def{onAction= setText lb "Alt-F2"}})
        ,("hk1",def{actionHotKey= hkCtrl SDL.KeycodeD, actionValue=def{onAction= setText lb "Ctrl-D"}})
        ]

    setAction gui "File" "Save" $ setText lb "Нажат пункт меню File/Save"
#else
    #error EXAMPLE_NUM is out of range
#endif

----- Поезные заметки
-- Fonts UbuntuMono-*.ttf loaded from
-- http://ru.fonts2u.com/download/ubuntu-mono.%D1%81%D0%B5%D0%BC%D0%B5%D0%B9%D1%81%D1%82%D0%B2%D0%BE
-- Fonts PT*.ttf loaded from
-- http://www.paratype.ru/public/


#if EXAMPLE_NUM == 3
exampleTextGrid :: MonadIO m => Widget -> Skin -> m (GuiWidget SimpleWidget)
exampleTextGrid parent _ = mkWidget (WidgetVisible .|. WidgetEnable .|. WidgetFocusable)
                                WidgetMarginNone SimpleWidget
                                parent defWidgFns{
        onCreate = \widget -> setWidgetCanvasRect widget (SDL.Rectangle zero (V2 500 800)) >>
                                 notifyParentAboutSize widget zero
        , onResizing= setWidgetRect
        ,onMouseButton = \widget motion mouseButton _clicks _point ->
            -- Нужно установить на виджет фокус что бы можно было прокручивать колесом мыши
            when ((mouseButton == SDL.ButtonLeft) && (motion==SDL.Pressed)) $ setWidgetFocus widget
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
                                drawStr fnt txtColor (P (V2 x y)) $ concat [show x,",",show y]
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

#if EXAMPLE_NUM == 5
mouseChkWidget :: MonadIO m => Widget -> Skin -> m (GuiWidget SimpleWidget)
mouseChkWidget parent _ = do
    rfState <- newMonadIORef Nothing
    mkWidget (WidgetVisible .|. WidgetEnable) (WidgetMarginXY 20 10) SimpleWidget parent
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
                Just p@(P v) -> do
                    setColor $ grayColor 255
                    let crossSz = 10
                    drawLine (p .-^ V2 crossSz 0) (p .+^ V2 crossSz 0)
                    drawLine (p .-^ V2 0 crossSz) (p .+^ V2 0 crossSz)
                    return $ show v
                _ -> return "No mouse"
            drawStr fnt (grayColor 255) (P (V2 5 5)) s
                                                            }
#endif

{- Ниже приведены виджеты не включённые в этот набор примеров. Вы можете их подключить или переделать самостоятельно -}

-- Цветной хеллоуворд                                                                }
exampleWidgetHelloWorld :: MonadIO m => Widget -> Skin -> m (GuiWidget SimpleWidget)
exampleWidgetHelloWorld parent _ = mkSimpleWidget (WidgetMarginXY 20 10) parent (noChildrenFns $ V2 0 60){
        onDraw= \ widget -> do
                visibleRect <- getVisibleRect widget
                setColor $ V4 0 0 255 0
                fillRect visibleRect
                setColor $ V4 255 0 0 0
                drawRect $ shrinkRect' 5 visibleRect
                fnt <- getFont ""
                drawStrAligned fnt AlignCenter (V4 255 255 255 0) DrawStrFine visibleRect "Привет, мир!"
                        }

-- отображение картинки, возможно с прозрачными областями
exampleWidgetPicture :: MonadIO m => Widget -> Skin -> m (GuiWidget SimpleWidget)
exampleWidgetPicture parent _ = mkSimpleWidget (WidgetMarginXY 20 10) parent (noChildrenFns $ V2 30 140){
        onDraw= \widget -> do
                    visibleRect <- getVisibleRect widget
                    setColor $ V4 255 255 255 0
                    fillRect visibleRect
                    drawTextureR "ScrollAreaArrBtns.png" zero
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
            tRed <- fromJust <$> renderStr fnt (V4 255 0 0 100) "Тест"
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
framesTest parent Skin{..} = mkSimpleWidget (WidgetMarginXY 20 10) parent (noChildrenFns $ V2 300 80){
            onDraw= \ widget -> do
                visibleRect@(SDL.Rectangle p0 _) <- getVisibleRect widget
                setColor bkColor
                fillRect visibleRect
                let rect0 = SDL.Rectangle (p0 .+^ V2 10 10)  (V2 50 50)
                    ligthColor = grayColor 240
                    darkColor = grayColor 180
                draw3DBorder ligthColor darkColor 1 rect0
                draw3DBorder ligthColor darkColor 2 $ rectMove rect0 (V2 70 0)
                setColor $ grayColor 0
                drawRoundBorder $ rectMove rect0 (V2 140 0)
                let arrH = 4
                drawArrow OrientationLeft  (P (V2 220 30)) arrH
                drawArrow OrientationUp    (P (V2 240 30)) arrH
                drawArrow OrientationRight (P (V2 260 30)) arrH
                drawArrow OrientationDown  (P (V2 280 30)) arrH
            }

