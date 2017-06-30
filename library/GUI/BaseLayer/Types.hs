{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module:      GUI.BaseLayer.Types
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <kolodeznydiver@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Определения основных взаимозависящих типов GUI.
-- Сведены в один файл что бы исключить взимную (циклическую) зависимость модулей.

module GUI.BaseLayer.Types where

import Data.Word
import Data.IORef
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T
import Data.Default
import qualified SDL
import GUI.BaseLayer.Depend0.Types
import GUI.BaseLayer.Depend0.BitFlags
import GUI.BaseLayer.Depend0.Cursor (CursorIx)
import GUI.BaseLayer.Depend0.Pipe (GuiPipes)
import GUI.BaseLayer.Depend1.Action
import GUI.BaseLayer.Depend1.Logging (GUILog)
import GUI.BaseLayer.Depend1.Resource (ResourceManager(..))
import GUI.BaseLayer.Depend1.Skin
import GUI.BaseLayer.Canvas.Types (TextureCache,Canvas)

-- | Тэг для битовых флагов состояния виджета
data WidgetOpts

-- | Тэг для битовых флагов состояния окна
data WindowOpts

-- | Тип для битовых флагов состояния виджета
type WidgetFlags = Flags WidgetOpts

-- | Тип для битовых флагов состояния окна
type WindowFlags = Flags WindowOpts

-- | Ссылка на виджет в дереве виджетов и в большинстве функций для извлечения информации и
-- манипуляций с виджетами.
type Widget     = IORef WidgetRecord

-- | Ссылка на окно GUI, используется в большинстве функций для извлечения информации и
-- манипуляций с окнами GUI.
type Window  = IORef WindowRecord

-- | Ссылка на структуру общей информации о GUI. Для GUI создаётся одна ссылка, по сути, singleton.
type Gui        = IORef GUIRecord

-- | Тип - обёртка для кода обмена данными между сообщениями через @onNotify@.
-- предназначен для пользовательского уровня, в GUI-SDL2 не используется.
newtype GuiNotifyCode = GuiNotifyCode { unGuiNotifyCode :: Int}
            deriving (Eq)

-- | Набор функций - обработчиков событий для виджета базового уровня (обобщённого виджета).
-- Первым аргументом в этих функциях всегда ссылка на виджет для которого они вызываются
data WidgetFunctions = WidgetFunctions  {
    -- | Вызывается после создания виджета и встраивания его в дерево виджетов.
    -- Должен вызывать @notifyParentAboutSize@ (непосредственно или через @onCreate@ ранее
    -- определённого набора функций 'WidgetFunctions').
    onCreate  :: forall m. MonadIO m => Widget -> m ()
    -- | Вызывается при удалении виджета. На момент вызова у виджета остаются его потомки но
    -- его предок, возможно, уже получил @onDestroy@.
    -- Удалять своих потомков вручную нельзя. Это сделает GUI.
    -- При удалении (под)дерева виджетов вначале вызывается @onDestroy@ для предков, потом для потомков.
  , onDestroy :: forall m. MonadIO m => Widget -> m ()
    -- | Только в @onDraw@ можно отрисовывать виджет функциями из "GUI.BaseLayer.Canvas".
    -- В @onDraw@ не следует обращаться к GUI для изменения состояния, только для извлечения информации.
  , onDraw :: forall m. MonadIO m => Widget -> Canvas m ()
    -- | Посылается к виджету когда для него была вызвана @markWidgetForRedraw@.
    -- В большинстве случаев никаких действий не требует.
  , onMarkForRedrawNotiy :: forall m. MonadIO m => Widget -> m ()
    -- | Уведомление посылаемое от виджета-потомка предку с предложением изменить свой размер.
    -- На предке лежит принятие решения о новом размере потомка, в том числе проверка, не запрашивается
    -- ли размер который уже есть. В этом случае изменять размер потомка, конечно, не стоит.
    -- При расчёте координат виджетов в layout-ах @onSizeChangedParentNotify@ и @onResizing@
    -- могут вызываться много раз пока всех не удовлетворит выбранный размер.
  , onSizeChangedParentNotify :: forall m. MonadIO m =>
        Widget  ->  -- ^ Виджет-предок (контейнер, часто layout).
        Widget  ->  -- ^ Виджет-потомок.
        GuiSize ->  -- ^ Предлагаемый размер включая margin. Одна или обе координаты могут быть <0.
                    -- Это означает запрос на выделения пространства, остающегося после размещения
                    -- других виджетов в layout-е (если layout такое поддерживает).
                    -- Если несколько виджетов запрашивают отрицательные размеры, то им выделяется
                    -- пространство пропорциональное модулю отрицательной координаты.
        m ()
    -- | Уведомление виджета о то что его размер или координаты изменены. См. @onSizeChangedParentNotify@
  , onResizing :: forall m. MonadIO m =>
        Widget  -> -- ^ Виджет, для которого послано уведомление.
        GuiRect -> -- ^ Новые координаты виджета включая margin.
                   -- Виджет должен уменьшить этот прямоугольник на свои margin и сохранить
                   -- в поле @widgetRect@ с помощью функций @simpleOnResizing@ и подобных.
                   -- Если виджет не скроллируемый, в те же значения устанавливаются и размеры поля
                   -- @widgetCanvasRect@.
        m ()
    -- | Уведомление виджета о то что курсор мыши вошёл в его координаты.
  , onGainedMouseFocus :: forall m. MonadIO m =>
        Widget   -> -- ^ Виджет, для которого послано уведомление.
        GuiPoint -> -- Точка расположения указателя в координатах виджета (без margin).
                    -- Левый верхний угол виджета __ P (V2 0 0) __.
        m ()
    -- | Уведомление виджета о то что курсор мыши переместился в пределах его координат.
    -- (Или когда виджет захватил мышь вызвав @setMouseCapturedWidget@)
  , onMouseMotion :: forall m. MonadIO m =>
        Widget -> -- ^ Виджет, для которого послано уведомление.
        [SDL.MouseButton] -> -- ^ Cписок нажатых кнопок мыши.
        GuiPoint -> -- ^ Точка расположения указателя в координатах виджета (см. @onGainedMouseFocus@).
        GuiSize -> -- ^ Насколько переместился указатель с прошлого сообщения.
        m ()
    -- | Уведомление виджета о то что одна из кнопок мыши быза нажата или отпущена когда
    -- курсор мыши находился в предеах его координат.
    -- (Или когда виджет захватил мышь вызвав @setMouseCapturedWidget@)
  , onMouseButton :: forall m. MonadIO m =>
        Widget -> -- ^ Виджет, для которого послано уведомление.
        SDL.InputMotion -> -- ^ Кнопка нажата = SDL.Pressed, кнопка отпущена = SDL.Released.
        SDL.MouseButton -> -- ^ Какая кнопка.
        Int             -> -- ^ Сколько раз щёлкнули кнопкой (одиночный или двойной клик).
        GuiPoint -> -- ^ Точка расположения указателя в координатах виджета (см. @onGainedMouseFocus@).
        m ()
    -- | Уведомление виджета о Прокрутки колеса мыши.
  ,  onMouseWheel :: forall m. MonadIO m =>
        Widget  -> -- ^ Виджет, для которого послано уведомление.
        GuiSize -> -- ^ На сколько позиций прокрутилось колесо.
                   -- ^ 2D координата на случай 2D колеса мыши. :)
        SDL.MouseScrollDirection -> -- ^ Инвертировано ли колесо мыши
                                    -- SDL.ScrollNormal | SDL.ScrollFlipped
                                    -- Возможно, можно изменить в каких то настройках, у меня не меняется.
        m ()
    -- | Уведомление виджета о то что курсор мыши переместился за пределы его координат.
  , onLostMouseFocus :: forall m. MonadIO m => Widget -> m ()
    -- | Уведомление виджета о то что он получил фокус. Т.е. к нему теперь пойдут сообщения от клавиатуры.
  , onGainedKeyboardFocus :: forall m. MonadIO m => Widget -> m ()
    -- | Уведомление виджета о то что он потерял фокус.
  , onLostKeyboardFocus :: forall m. MonadIO m => Widget -> m ()
    -- | Уведомление виджета в фокусе о вводе символа с клавиатуры. При нажатии клавиш посылается либо
    -- @onTextInput@, либо @onKeyboard@.
  , onTextInput :: forall m. MonadIO m =>
        Widget -> -- ^ Виджет, для которого послано уведомление.
        T.Text -> -- ^ 'T.Text', а не 'Char' потому что при вводе на других, более японских языках,
                  -- могут сформироваться сразу несколько символов. Для Русского, English и подобных
                  -- будет строка ровно из одного символа. (Это не я придумал, так в SDL).
        m ()
    -- | Уведомление виджета в фокусе о нажатии чего то, что не означает ввод символа : ^C или F3.
  ,  onKeyboard :: forall m. MonadIO m =>
        Widget -> -- ^ Виджет, для которого послано уведомление.
        SDL.InputMotion -> -- ^ Кнопка нажата = SDL.Pressed, кнопка отпущена = SDL.Released.
        Bool -> -- ^ Если True, то это повтор при удержании клавишы нажатой.
        SDL.Keycode -> -- ^ Код клавишы, например SDL.KeycodeA, SDL.KeycodeF1, SDL.KeycodeLeft.
        SDL.KeyModifier -> -- ^ Какие клавишы типа SDL.keyModifierLeftShift сейчас нажаты.
        m ()
    -- | Уведомление всех виджетов о изменении кода состояния программы.
    -- (Код состояния программы хранится по ссылке Gui и изменяется пользовательским кодом.)
  , onGuiStateChange :: forall m. MonadIO m =>
        Widget -> -- ^ Виджет, для которого послано уведомление.
        GuiState -> -- ^ Новый код состояния программы.
        m ()
    -- | Пользовательское уведомление от одного виджета другому с произвольным кодом.
    -- В базовом слое GUI не предполагается использовать. В настоящий момент не используется в GUI
    -- вообще. Предназначено для пользовательского кода. Не следует путать с обработчиками
    -- событий устанавливаемых через @setAction@ или потокобезопасных обработчиков pipe-сообщений @newGuiPipe@.
  , onNotify :: forall m. MonadIO m =>
        Widget -> -- ^ Виджет, для которого послано уведомление.
        GuiNotifyCode -> -- ^ Код уведомления назначенный отправителем.
        Maybe Widget -> -- ^ Возможно, виджет - отправитель.
        m ()
  }

-- | Обработчики события базового виджета по умолчанию - все пустые действия.
instance Default WidgetFunctions where
    def = WidgetFunctions       { onCreate = \_ -> return ()
                                , onDestroy = \_ -> return ()
                                , onDraw = \_ -> return ()
                                , onSizeChangedParentNotify = \_ _ _ -> return ()
                                , onMarkForRedrawNotiy = \_ -> return ()
                                , onResizing = \_ _ -> return ()
                                , onGainedMouseFocus = \_ _ -> return ()
                                , onMouseMotion = \_ _ _ _ -> return ()
                                , onMouseButton = \_ _ _ _ _ -> return ()
                                , onMouseWheel = \_ _ _ -> return ()
                                , onLostMouseFocus = \_ -> return ()
                                , onGainedKeyboardFocus = \_ -> return ()
                                , onLostKeyboardFocus = \_ -> return ()
                                , onTextInput = \_ _ -> return ()
                                , onKeyboard = \_ _ _ _ _ -> return ()
                                , onGuiStateChange = \_ _ -> return ()
                                , onNotify = \_ _ _ -> return ()
                                }

-- | Наличие в окне виджета с особым состоянием. В настоящий момент такое особое состояние может быть
-- только у виджета захватившего мышь в окне. Тип только для BaseLayer.
data SpecStateWidget    = WidgetNoSpecState
                        | WidgetStateMouseCaptured Widget

-- | Контейнер хранения виджетов-потомков (дочерних виджетов) в дереве виджетов.  Тип только для BaseLayer.
type GuiWidgetCollection = V.Vector Widget

-- | Контейнер хранения открытых SDL окон с доступом по индексу. (Индекс присваивается внутри SDL.).
--  Тип только для GUI.BaseLayer.
type GuiWindowCollection = Map.Map GuiWindowIx Window

-- | Тип кода сообщаемого обработчику @winOnClosed@ после закрытия окна.
newtype WindowRetcode = WindowRetcode { unWindowRetcode :: Int }

-- | Структура (запись, record) хранящая параметры обобщённого виджета базового уровня.
data WidgetRecord = WidgetRecord {
    windowOfWidget :: Window -- ^ Ссылка на своё окно.
  , parentWidget :: ~Widget -- ^ Родительский виджет в дереве виджетов окна.
                            -- В специально, корневом, виджете эта ссылка указывает на себя.
  , cildrenWidgets :: GuiWidgetCollection -- ^ Виджеты - потомки.
  , widgetRect :: GuiRect -- ^ Координаты виджета без учёта margin вокруг виджета в координатах
                          -- родительского виджета (виджета-предка).
  , widgetCanvasRect :: GuiRect -- ^ Прямоугольник виртуального пространства отображения.
                                -- Для нескроллируемых виджетов его начальная точка __ P (V2 0 0 ) __,
                                -- а размер должен быть равен размеру @widgetRect@.
                                -- Для скроллируемого начальная точка означает смещение области
                                -- отображения в виртуальном пространстве.
  , widgetMargin :: GuiMargin -- ^ поля (margin) вокруг виджета. В этих полях виджет не может рисовать,
                              -- они нужны для визаульного отделения виджетов друг от друга.
  , widgetFlags :: WidgetFlags -- ^ Битовые флаги виджета. См. "GUI.BaseLayer.Widget".
  , widgetCursor :: CursorIx -- ^ Курсор, который должен отображаться когда указатель мыши находится над
                             -- виджетом (если задан).
  , widgetFns :: WidgetFunctions -- ^ Набор функций - обработчиков событий виджета.
  }

-- | Структура (запись, record) хранящая параметры окна GUI (одно окно GUI соответствует одному окну SDL).
data WindowRecord = WindowRecord {
    guiOfWindow :: Gui -- ^ Ссылка на 'GUIRecord', одна на все окна.
  , winSDL :: SDL.Window -- ^ Окно SDL.
  , winRenderer :: SDL.Renderer -- ^ Renderer окна SDL.
  , mainWidget :: ~Widget -- ^ Корневой, специальный, виджет окна. Существет с момента создания окна.
                          -- его нельзя удалять и перенастраивать.
  , winFlags :: WindowFlags -- ^ Битовые флаги окна. См. "GUI.BaseLayer.RedrawWindow".
  , specStateWidget :: SpecStateWidget -- ^ Ссылка на виджет специального состояния, если есть.
  , widgetUnderCursor :: Maybe Widget -- ^ Виджет на котором находится сейчас курсор.
  , focusedWidget :: Maybe Widget -- ^ Виджет, имеющий сейчас клавиатурный фокус.
  , curWinCursor :: CursorIx -- ^ Индекс текущего курсора в окне.
  , winBuffer :: SDL.Texture -- ^ Буфер (растр) отображения нижнего (основного) слоя в окне.
  , winProxyTexture :: SDL.Texture -- ^ Буфер (растр) размером 1х1 пиксель. Используется
                                   -- функциями @GUI.BaseLayer.Core.runProxyWinCanvas@,
                                   -- @GUI.BaseLayer.Core.runProxyCanvas@.
  , winMainMenu :: Maybe Widget -- ^ Ссылка на виджет @horizontalMenu@, если он есть в окне.
  , winTextureCache :: TextureCache -- ^ Кеш текстур данного окна.
  , winFgWidget :: ~Widget -- ^ -- Корневой виджет дерева foreground виджетов, отрисовываемых
                                -- поверх основного, нижнего слоя. Такой же специальный виджет, как и
                                -- @mainWidget@.
                                -- Предназначен для отображения всплывающих подсказок,
                                -- элементов интерфейса в процессе перетаскивания, выпадающих
                                -- списков и подобного. (in the future).
  , winFgBuffer :: SDL.Texture -- ^ Буфер (растр) отображения верхнего (foreground) слоя в окне.
    -- | Код сообщаемый обработчику @winOnClosed@ после закрытия окна.
  , winRetcode :: WindowRetcode
    -- | Обработчик события вызывается при попытке закрыть окно средствами ОС (нажание на [x] и подобные.
    -- окно закрывается если функция возвращает True.
  , winCloseConfirm :: forall m. MonadIO m => Window -> m Bool
    -- | Обработчик события  вызывается после закрытия окна и получает установленный ранее код возврата.
  , winOnClosed :: forall m. MonadIO m => WindowRetcode -> m ()
  }

-- | Структура (запись, record) хранящая параметры целого GUI.
data GUIRecord = GUIRecord {
    guiWindows :: GuiWindowCollection -- ^ все откртыте окна GUI.
  , guiSkin :: Skin -- ^ GUI Skin.
  , userEventCodeBase :: Word32 -- ^ код для пользовательсокого сообщения запрошенный от SDL
                                -- при инициализации.
  , resourceManager :: ResourceManager -- ^ Менеджер ресурсов (картинок, шрифтов, и курсоров).
  , guiActions :: Actions -- ^ Action-ы и HotKey-и приложения.
  , guiState :: GuiState -- ^ Текущий код состояния приложения (устанавливается в пользовательском коде
                         -- через @GUI.BaseLayer.Core.setGuiState@).
  , guiUnique :: Int -- ^ Инкрементируемое поле для создания уникальных кодов.
  , guiPipes :: GuiPipes -- ^ Потокобезопасные каналы, Cм. "GUI.BaseLayer.Pipe.GuiPipe".
  , guiLog :: GUILog -- ^ Журнал приложения.
  , guiModalWins :: [GuiWindowIx] -- ^ Стек модальных окон.
  }

