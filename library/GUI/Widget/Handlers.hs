{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

-- |
-- Module:      GUI.Widget.Handlers
-- Copyright:   (c) 2017-2020 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Заготовки наборов - обработчиков событий, используемые в разных виджетах.
-- (Повторное использование кода рекомендуется лучшими ...)

module GUI.Widget.Handlers(
    noChildrenFns,colorRectFns,grayRectFns
    ,OnClickHandler,OnClickOneArgHandler,AcceptPointHandler
    ,ClickableHelper'(..),ClickableHelper(..)
    ,clickableHelper',clickableHelper
    ,MouseAnimatedClickableHelper'(..),MouseAnimatedClickableHelper(..)
    ,mouseAnimatedClickableHelper',mouseAnimatedClickableHelper
    -- * Упрощённый вариант создания виджетов без коррекцци margins при инициализации.
    ,mkFormWidget
        ) where

import Control.Monad.IO.Class
import Control.Monad
import Data.Maybe
import Data.IORef
import Control.Monad.Extra
import Data.Default
import qualified SDL
import GUI
--import GUI.Widget.Types

-- | Набор обработчиков базового виджета, годится для большинства виджетов
-- не имеющих дочерних виджетов.
noChildrenFns :: GuiSize -> -- ^ Размеры виджета без полей.
                 WidgetFunctions
noChildrenFns initInsideSz = def{
    onCreate = \widget -> notifyParentAboutSize widget initInsideSz
    ,onResizing= \widget -> void . extendableOnResizing initInsideSz widget
                             }
{-# INLINEABLE noChildrenFns #-}

-- | Годится для демонстрации - отрисовки прямоугольника заданного размера и цвета.
colorRectFns :: GuiSize -> GuiColor -> WidgetFunctions
colorRectFns sz color = (noChildrenFns sz){
    onDraw= \widget -> setColor color >> getVisibleRect widget >>= fillRect
                                           }
-- | Годится для демонстрации - отрисовки прямоугольника заданного размера и цвета серой шкалы.
grayRectFns:: GuiSize -> ColorComponent -> WidgetFunctions
grayRectFns sz = colorRectFns sz . grayColor
{-# INLINE grayRectFns #-}

type OnClickHandler = forall m. MonadIO m => Widget -> Bool -> GuiPoint -> m ()

type OnClickOneArgHandler = forall m. MonadIO m => Widget -> m ()

type AcceptPointHandler = forall m. MonadIO m => Widget -> GuiPoint -> m Bool


-- | Запись, которая пригодится для виджета который можно "кликать" указателем мыши или
-- по клавишам Enter и пробел когда виджет в фокусе.
-- Она генерируется @сlickableHandler@.
newtype ClickableHelper' = ClickableHelper' {
      -- | Событие, которое отреагирует и на нажатие и на отпускание мыши.
      -- А так же на Enter или пробел если виджет в фокусе.
--     clickableOnClick' :: OnClickHandler -- forall m. MonadIO m => Widget -> Bool -> GuiPoint -> m ()
      -- | Функции - обработчики событий базового виджета которые предлагается использовать.
      -- Никто не мешает их далее модифицировать.
     clickableFs' :: WidgetFunctions
                                         }
-- | Функция, создающая 'ClickableHelper\'' из которого просто делается виджет без потомков
-- реагирующий на "клик" мышью или клавиши Enter/пробел.
clickableHelper' :: forall m. MonadIO m =>
        GuiSize -> -- ^ Размеры виджета без полей.
        OnClickHandler -> -- ^ Обработчик клика.
        AcceptPointHandler -> -- ^ Предикат разрешающий кликнуть мышью
        m ClickableHelper'
clickableHelper' sz onClickAction isClickablePredicate =
    return (ClickableHelper' (noChildrenFns sz){
        onKeyboard = \widget motion _repeated keycode km ->
                        let shiftCtrlAlt = getShftCtrlAlt km in
                        when (shiftCtrlAlt == ShiftCtrlAlt False False False &&
                           (motion==SDL.Pressed) &&
                           (isEnterKey keycode || keycode == SDL.KeycodeSpace)) $ -- do
{-                                sDbg <- widgetCoordsToStr widget
                                ms <- readMonadIORef mouseState
                                liftIO $ putStrLn $ concat ["ClickableFns.onKeyboard ",
                                    show (motion==SDL.Pressed), "    ", sDbg, "   ", show ms] -}
                                onClickAction widget True KbdClickSpecPoint
{-                                ms2 <- readMonadIORef mouseState
                                liftIO $ putStrLn $ concat ["ClickableFns.onKeyboard after ",
                                     show ms2] -}
        ,onMouseButton = \widget motion mouseButton _clicks pnt ->
            when (mouseButton == SDL.ButtonLeft) $
                whenM (allWidgetFlags widget WidgetEnable) $
                    whenM (isClickablePredicate widget pnt) $
                        onClickAction widget (motion==SDL.Pressed) pnt
                               })


-- | Запись, которая пригодится для виджета который можно "кликать" указателем мыши или
-- по клавишам Enter и пробел когда виджет в фокусе.
-- Она генерируется @сlickableHandler@.
data ClickableHelper = ClickableHelper {
      -- | Событие, которое отреагирует и на нажатие и на отпускание мыши.
      -- А так же на Enter или пробел если виджет в фокусе.
      clickableOnClick :: IORef NoArgAction
      -- | Функции - обработчики событий базового виджета которые предлагается использовать.
      -- Никто не мешает их далее модифицировать.
     ,clickableFs :: WidgetFunctions
                                         }
-- | Функция, создающая 'ClickableHelper' из которого просто делается виджет без потомков
-- реагирующий на "клик" мышью или клавиши Enter/пробел.
-- Например, "GUI.Widget.CheckBox".
-- От @clickableHelper'@ отличается типом обработчика клика.
clickableHelper :: forall m. MonadIO m =>
        GuiSize -> -- ^ Размеры виджета без полей.
        OnClickOneArgHandler -> -- ^ Обработчик клика.
        AcceptPointHandler -> -- ^ Предикат разрешающий кликнуть мышью
        m ClickableHelper
clickableHelper sz onClickAction isClickablePredicate = do
    onCLick' <- newMonadIORef $ NoArgAction $ return ()
    let onClickAction' widget pressed _pnt = when pressed $ do
            markWidgetForRedraw widget
            join $ noArgAction <$> readMonadIORef onCLick'
            onClickAction widget
    ClickableHelper'{..} <- clickableHelper' sz onClickAction' isClickablePredicate
    return (ClickableHelper onCLick' clickableFs')


-- | Запись, которая пригодится для виджета отслеживающего и анимирующего своё положение
-- в зависимости от положения мыши и кнопки Enter, когда виджет в фокусе.
-- Она генерируется @mouseAnimatedClickableHelper'@.
data MouseAnimatedClickableHelper' = MouseAnimatedClickableHelper' {
      -- | Состояние мыши относительно виджета. См. "GUI.Widget.Types".
      mouseAnimatedClickableMouseState' :: IORef WidgetMouseState
      -- | Функции - обработчики событий базового виджета которые предлагается использовать.
      -- Никто не мешает их далее модифицировать.
    , mouseAnimatedClickableFs' :: WidgetFunctions
                                                               }

-- | Функция, создающая 'MouseAnimatedHelper' из которого просто делается виджет без потомков.
-- Например, "GUI.Widget.TH.LinearTrackBar".
mouseAnimatedClickableHelper' :: forall m. MonadIO m =>
        GuiSize -> -- ^ Размеры виджета без полей.
        OnClickHandler -> -- ^ Обработчик клика.
        AcceptPointHandler -> -- ^ Предикат разрешающий кликнуть мышью
        m MouseAnimatedClickableHelper'
mouseAnimatedClickableHelper' sz onClickAction isClickablePredicate = do
    mouseState <- newMonadIORef WidgetMouseOut
    let onClickAction' widget pressed pnt = do
                writeMonadIORef mouseState $ if | pressed -> WidgetMousePressed
                                                | pnt == KbdClickSpecPoint -> WidgetMouseOut
                                                | otherwise -> WidgetMouseIn
                markWidgetForRedraw widget
                onClickAction widget pressed pnt
    ClickableHelper'{..} <- clickableHelper' sz onClickAction' isClickablePredicate
    return (MouseAnimatedClickableHelper' mouseState clickableFs'{
        onGainedMouseFocus = \widget _pnt ->
            writeMonadIORef mouseState WidgetMouseIn >> markWidgetForRedraw widget
        ,onLostMouseFocus = \widget ->
            writeMonadIORef mouseState WidgetMouseOut >> markWidgetForRedraw widget
                               })

-- | Запись, которая пригодится для виджета отслеживающего и анимирующего своё положение
-- в зависимости от положения мыши и кнопки Enter, когда виджет в фокусе.
-- Она генерируется @mouseAnimatedClickableHelper@.
data MouseAnimatedClickableHelper = MouseAnimatedClickableHelper {
      -- | Состояние мыши относительно виджета. См. "GUI.Widget.Types".
      mouseAnimatedClickableMouseState :: IORef WidgetMouseState
      -- | Событие, которое отреагирует на нажатие мыши и кнопки Enter если виджет будет в фокусе.
    , mouseAnimatedClickableAction :: IORef NoArgAction
      -- | Функции - обработчики событий базового виджета которые предлагается использовать.
      -- Никто не мешает их далее модифицировать.
    , mouseAnimatedClickableFs :: WidgetFunctions
                                                               }

-- | Функция, создающая 'MouseAnimatedClickableHelper' из которого просто делается виджет без потомков.
-- Например, @GUI.Widget.Button.button@.
mouseAnimatedClickableHelper :: forall m. MonadIO m =>
        GuiSize -> -- ^ Размеры виджета без полей.
        OnClickOneArgHandler -> -- ^ Обработчик клика.
        AcceptPointHandler -> -- ^ Предикат разрешающий кликнуть мышью
        m MouseAnimatedClickableHelper
mouseAnimatedClickableHelper sz onClickAction isClickablePredicate = do
    onCLick' <- newMonadIORef $ NoArgAction $ return ()
    let onClickAction' widget b _p = when b ((join $ noArgAction <$> readMonadIORef onCLick')
                                        >> onClickAction widget {- b p -})
    MouseAnimatedClickableHelper'{ mouseAnimatedClickableMouseState' = mouseState
                                 , mouseAnimatedClickableFs' = fns }
                 <- mouseAnimatedClickableHelper' sz onClickAction' isClickablePredicate
    return (MouseAnimatedClickableHelper mouseState onCLick' fns)


-- | Эта функция используется в функциях создания конкретных, пользовательских, виджетов на формах.
-- В отличии от @GUI.BaseLayer.Widget.mkWidget@ поля виджета определяются по FormItemWidgetDef и Skin
mkFormWidget :: MonadIO m => FormItemWidgetDef -> -- ^ Общие настройки для всех виджетов для форм.
                             WidgetFlags -> -- ^ Флаги виджета.
                             Skin -> -- ^ Skin
                             (GuiMargin -> GuiMargin) -> -- ^ Функция возможной корркции полей при инициализации.
                                                         -- часто @id@.
                             a -> -- ^ Данные виджета верхнего уровня.
                             Widget ->  -- ^ Родительский виджет
                             WidgetFunctions -> -- ^ запись с функциями - обработчиками событий
                                                        -- базового уровня.
                             m (GuiWidget a)
mkFormWidget formDef fl skin margFn =
    mkWidget fl (fromMaybe (
                    let (MarginLTRB l t r b) = margFn $ marginToLTRB $ formItemsMargin skin
                    in  WidgetMarginLTRB l t r b
                           ) $ formItemMargin formDef)

