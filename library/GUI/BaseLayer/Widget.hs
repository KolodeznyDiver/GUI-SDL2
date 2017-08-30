{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      GUI.BaseLayer.Widget
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <kolodeznydiver@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Типы и функции относящиеся к 'Widget' и 'GuiWidget'.
-- Многие функции, относящиеся к 'Widget' перенесены в другие модули для разрешения циклических
-- зависимостей между модулями.

module GUI.BaseLayer.Widget(
     -- * Виджет Базового уровня.
     -- ** Низкоуровневые функции доступа к полям записи виджета.
     getWidgetParent,setWidgetParent,getWidgetRect,setWidgetRect,getWidgetCanvasRect,setWidgetCanvasRect
    ,getWidgetFns,setWidgetFns,getWidgetCursorIx,setWidgetCursorIx,getWidgetWindow
     -- ** Флаги виджета
     ,pattern WidgetNoFlags, pattern WidgetRedrawFlag, pattern WidgetSelectable, pattern WidgetEnable
     ,pattern WidgetVisible ,pattern WidgetFocusable,pattern WidgetTabbed, pattern WidgetMouseWheelControl
     ,pattern WidgetFocused
     ,getWidgetFlags,setWidgetFlags,widgetFlagsAdd,widgetFlagsRemove,removeWidgetFlags
     ,widgetFlagsAddRemove,allWidgetFlags',allWidgetFlags,anyWidgetFlags
     ,isWidgetMarkedForRedrawing',isWidgetMarkedForRedrawing,clearWidgetRedrawFlag
     -- ** Поля (margin) виджета и функции использующие их.
     ,getWidgetMargin,setWidgetMargin,setWidgetMargin',getWidgetMarginSize,getWidgetRectWithMargin
     ,setWidgetRectWithMarginShrink,calcWidgetSizeWithMargin,widgetResizingIfChanged
     ,getWidgetVisibleRect,getVisibleRect
     -- ** Функции использующие вектор дочерих виджетов.
     ,getWidgetChildrenCount,getChildWidgetIx',getChildWidgetIx,getWidgetChild,swapChildWidgets
     -- *** Map-ы по вектору виджетов.
     ,mapByWidgetChildren,imapByWidgetChildren,mapByWidgetChildren_,imapByWidgetChildren_
     -- *** Свёртки по вектору виджетов.
    ,foldByWidgetChildren,foldByWidgetChildren',ifoldByWidgetChildren,ifoldByWidgetChildren'
    ,foldByWidgetChildren_,foldByWidgetChildren'_,ifoldByWidgetChildren_,ifoldByWidgetChildren'_
    -- ** Функции извлечения информации из родительского виджета.
    ,getWidgetParentIx,getWidgetParentFns,getPrevNextWidgets
    -- ** Оперции с деревом виджетов.
    ,forEachWidgets,isMainWidget,getWinMainWidget
    ,findWidgetInTreeForward,findWidgetInTreeBackward,findNextTabbedWidget,findPrevTabbedWidget
    -- ** Доступ к данным 'Window' или 'Gui' через виджет.
    ,getGuiFromWidget,getSkinFromWidget
    -- ** Использование виджета базового уровня
    ,markWidgetForRedraw
    -- *** Изменение координат виджета.
    ,moveWidget,resizeWidget,resizeWidgetWithCanvas,getWidgetCoordOffset
    ,notifyParentAboutSize,simpleOnResizing,extendableOnResizing
    -- *** Поиск и преобразование из клиентских координат в координаты виджета.
    ,coordToWidget,mouseToWidget
    -- ** Логирование и обработка прерываний в контексте виджета.
    ,logPutLnWidget,logOnErrInWidget',logOnErrInWidget
    -- ** Отладочные функции вывода парметров виджета(ов) в виде строки.
    ,widgetCoordsToStr,showWidgets,showWidgetsFromMain,showWinWidgets
    -- * Виджет пользовательского уровня.
    ,GuiWidget(..),setWidgetFlag,enableWidget,visibleWidget,fnsCorrectionForTransparent
    -- ** Простейший виджет пользовательского уровня не имеюший своих параметров.
    ,SimpleWidget(..),mkSimpleWidget
    --  * Создание виджетов.
    ,mkWidget',mkWidget,createWidget
                 ) where

import Control.Monad.Trans.Class
import Data.Bits
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Control.Monad
import Control.Monad.IO.Class (MonadIO,liftIO)
import qualified TextShow as TS
import qualified SDL
import SDL.Vect
import GUI.BaseLayer.Depend0.Types
import GUI.BaseLayer.Depend0.Auxiliaries
import GUI.BaseLayer.Depend0.Ref
import GUI.BaseLayer.Depend0.BitFlags
import GUI.BaseLayer.Depend0.Cursor
import GUI.BaseLayer.Depend1.Geometry
import qualified GUI.BaseLayer.Depend1.Logging as L (logPutLn,logOnSomeException)
import GUI.BaseLayer.Depend1.Skin
import GUI.BaseLayer.Types
import GUI.BaseLayer.Canvas.Types (Canvas)
import GUI.BaseLayer.Window
import GUI.BaseLayer.GUIRecord

pattern WidgetNoFlags :: WidgetFlags
pattern WidgetRedrawFlag :: WidgetFlags
pattern WidgetSelectable :: WidgetFlags
pattern WidgetEnable :: WidgetFlags
pattern WidgetVisible :: WidgetFlags
pattern WidgetFocusable :: WidgetFlags
pattern WidgetMouseWheelControl :: WidgetFlags
pattern WidgetTabbed :: WidgetFlags
pattern WidgetFocused :: WidgetFlags
                                    --  5432109876543210
pattern WidgetNoFlags        = (Flags 0x0000000000000000) :: WidgetFlags
pattern WidgetRedrawFlag     = (Flags 0x0000000000000001) :: WidgetFlags
pattern WidgetVisible        = (Flags 0x0000000000000002) :: WidgetFlags
pattern WidgetEnable         = (Flags 0x0000000000000004) :: WidgetFlags
pattern WidgetFocusable      = (Flags 0x0000000000000008) :: WidgetFlags
pattern WidgetMouseWheelControl      = (Flags 0x0000000000000010) :: WidgetFlags
pattern WidgetFocused          = (Flags 0x0000000000000020) :: WidgetFlags
pattern WidgetSelectable     = (Flags 0x0000000000000040) :: WidgetFlags
pattern WidgetTabbed         = (Flags 0x0000000000000080) :: WidgetFlags

-- | Возвращает родительский виджет в дереве виджетов окна.
getWidgetParent:: MonadIO m => Widget -> m Widget
getWidgetParent = fmap parentWidget . readMonadIORef
{-# INLINE getWidgetParent #-}

-- | Устанавливает родительский виджет в параметрах текущего виджета.
-- Список дочерних виджетов у будущего родительского виджета не меняется.
-- Добавить виджет туда понадобится отдельно. Вы должны знать что вы делаете!
setWidgetParent:: MonadIO m => Widget -> -- ^ виджет для которого устанавливается родитель.
                               Widget -> -- ^ виджет - новый родитель.
                               m ()
setWidgetParent widget parent = modifyMonadIORef' widget (\w -> w{parentWidget=parent})
{-# INLINE setWidgetParent #-}

-- | Возвращает границы отрисовываемой области виджета в координатах родительского виджета.
getWidgetRect:: MonadIO m => Widget -> m GuiRect
getWidgetRect = fmap widgetRect . readMonadIORef
{-# INLINE getWidgetRect #-}

-- | Устанавливает границы отрисовываемой области виджета в координатах родительского виджета.
setWidgetRect:: MonadIO m => Widget -> GuiRect -> m ()
setWidgetRect widget r = modifyMonadIORef' widget (\w -> w{widgetRect=r})
{-# INLINE setWidgetRect #-}

-- | Возвращает границы канвы (в данном случае виртуальной области которую занимает виджет).
-- Для нескроллируемых виджетов может начинаться с точки zero и иметь размеры @getWidgetRect@.
-- Для скроллируемых размер - размер виртуальной области рисования, а начальная точка прямоугольника -
-- смещение окна видимой на экране области относительно виртуальной области рисования.
getWidgetCanvasRect:: MonadIO m => Widget -> m GuiRect
getWidgetCanvasRect = fmap widgetCanvasRect . readMonadIORef
{-# INLINE getWidgetCanvasRect #-}

-- | Устанавливает границы канвы (в данном случае виртуальной области которую занимает виджет).
setWidgetCanvasRect:: MonadIO m => Widget -> GuiRect -> m ()
setWidgetCanvasRect widget r = modifyMonadIORef' widget (\w -> w{widgetCanvasRect=r})
{-# INLINE setWidgetCanvasRect #-}

-- | Возвращает запись с набором обработчиков виджета базового уровня
getWidgetFns:: MonadIO m => Widget -> m WidgetFunctions
getWidgetFns = fmap widgetFns . readMonadIORef
{-# INLINE getWidgetFns #-}

-- | Устанавливает новую запись с набором обработчиков виджета базового уровня.
setWidgetFns:: MonadIO m => Widget -> WidgetFunctions -> m ()
setWidgetFns widget fs = modifyMonadIORef' widget (\w -> w{widgetFns=fs})
{-# INLINE setWidgetFns #-}

-- | Возвращает индекс курсора который должен отображаться когда указатель мыши находится над виджетом.
getWidgetCursorIx:: MonadIO m => Widget -> m CursorIx
getWidgetCursorIx = fmap widgetCursor . readMonadIORef
{-# INLINE getWidgetCursorIx #-}

-- | Устанавливает индекс курсора.
setWidgetCursorIx:: MonadIO m => Widget -> CursorIx -> m ()
setWidgetCursorIx widget ix = modifyMonadIORef' widget (\w -> w{widgetCursor=ix})
{-# INLINE setWidgetCursorIx #-}

-- | Возвращает ссылку на окно в котором находится виджет.
getWidgetWindow:: MonadIO m => Widget -> m Window
getWidgetWindow = fmap windowOfWidget . readMonadIORef
{-# INLINE getWidgetWindow #-}

-- | Возвращает все флаги виджета
getWidgetFlags:: MonadIO m => Widget -> m WidgetFlags
getWidgetFlags = fmap widgetFlags .  readMonadIORef
{-# INLINE getWidgetFlags #-}

-- | Устанавливает все флаги виджета.  Вы должны знать что вы делаете!
setWidgetFlags:: MonadIO m => Widget -> WidgetFlags -> m ()
setWidgetFlags widget fl = modifyMonadIORef' widget (\x -> x{widgetFlags=fl})
{-# INLINE setWidgetFlags #-}

-- | Добавляет флаг или несколько флагов объединённых по (.|.) к полю флагов виджета.
widgetFlagsAdd:: MonadIO m => Widget -> WidgetFlags -> m ()
widgetFlagsAdd widget add = modifyMonadIORef' widget (\x -> x{widgetFlags=widgetFlags x .|. add})
{-# INLINE widgetFlagsAdd #-}

-- | Удаляет флаг или несколько флагов объединённых по (.|.) из полю флагов виджета.
widgetFlagsRemove:: MonadIO m => Widget -> WidgetFlags -> m ()
widgetFlagsRemove widget rmv = modifyMonadIORef' widget (\x -> x{widgetFlags=widgetFlags x .&. complement rmv})
{-# INLINE widgetFlagsRemove #-}

-- | Чистая функция для удаления флагов из поля флагов виджета.
removeWidgetFlags:: WidgetFlags -> WidgetFlags -> WidgetFlags
removeWidgetFlags fl rmv = fl .&. complement rmv
{-# INLINE removeWidgetFlags #-}

-- | Одновременно добавляет и удаляет флаги из полю флагов виджета.
widgetFlagsAddRemove:: MonadIO m => Widget -> -- ^ Виджет
                                    WidgetFlags -> -- ^ Добавляемые флаги.
                                    WidgetFlags -> -- ^ удаляемые флаги.
                                    m ()
widgetFlagsAddRemove widget add rmv =
    modifyMonadIORef' widget (\x -> x{widgetFlags=(widgetFlags x .&. complement rmv) .|. add})
{-# INLINE widgetFlagsAddRemove #-}

-- | Проверяет, все ли из указанных флагов установлены.
-- Если задан только один флаг - эквивалентна проверки флага.
-- Функция принимает запись виджета 'WidgetRecord', а не ссылку на эту запись Widget.
-- Для оптимизации, когда запись по ссылке уже получена.
allWidgetFlags':: WidgetRecord -> WidgetFlags -> Bool
allWidgetFlags' w fl = fl == (fl .&. widgetFlags w)
{-# INLINE allWidgetFlags' #-}

-- | Проверяет, все ли из указанных флагов установлены.
-- Если задан только один флаг - эквивалентна проверки флага.
allWidgetFlags:: MonadIO m => Widget -> WidgetFlags -> m Bool
allWidgetFlags widget fl = (`allWidgetFlags'` fl) <$> readMonadIORef widget
{-# INLINE allWidgetFlags #-}

-- | Возвращает True если любой из указанных флагов установлен.
-- Если задан только один флаг - эквивалентна проверки флага.
anyWidgetFlags:: MonadIO m => Widget -> WidgetFlags -> m Bool
anyWidgetFlags widget fl = ((WidgetNoFlags /=) . (fl .&.) . widgetFlags) <$> readMonadIORef widget
{-# INLINE anyWidgetFlags #-}

-- | Помечен ли виджет для перерисовки в следующий раз.
-- Функция принимает запись виджета 'WidgetRecord', а не ссылку на эту запись Widget.
-- Для оптимизации, когда запись по ссылке уже получена.
isWidgetMarkedForRedrawing':: WidgetRecord -> Bool
isWidgetMarkedForRedrawing' = (WidgetRedrawFlag ==) . (WidgetRedrawFlag .&.) . widgetFlags
{-# INLINE isWidgetMarkedForRedrawing' #-}

-- | Помечен ли виджет для перерисовки в следующий раз.
isWidgetMarkedForRedrawing:: MonadIO m => Widget -> m Bool
isWidgetMarkedForRedrawing = fmap isWidgetMarkedForRedrawing' . readMonadIORef
{-# INLINE isWidgetMarkedForRedrawing #-}

-- | Сброс пометки виджета для перерисовки.
-- Замечание: Пометка виджета для перерисовки выполняет одновременно и пометку для перерисовки его окна.
-- Т.е. эта функция не восстанавливает состояние после @GUI.BaseLayer.Core.markWidgetForRedraw@
-- Для использования внутри /GUI.BaseLayer/.
clearWidgetRedrawFlag:: MonadIO m => Widget -> m ()
clearWidgetRedrawFlag widget = widgetFlagsRemove widget WidgetRedrawFlag
{-# INLINE clearWidgetRedrawFlag #-}

-- | Возвращает поля виджета.
getWidgetMargin:: MonadIO m => Widget -> m GuiMargin
getWidgetMargin = fmap widgetMargin . readMonadIORef
{-# INLINE getWidgetMargin #-}

-- | Устанавливает поля виджета из типа 'GuiMargin'.
setWidgetMargin:: MonadIO m => Widget -> GuiMargin -> m ()
setWidgetMargin widget m = modifyMonadIORef' widget (\w -> w{widgetMargin=m})
{-# INLINE setWidgetMargin #-}

-- | Устанавливает поля виджета из типа 'WidgetMargin'.
setWidgetMargin':: MonadIO m => Widget -> WidgetMargin -> m ()
setWidgetMargin' widget = setWidgetMargin widget . marginToLTRB
{-# INLINE setWidgetMargin' #-}


-- | Cуммирует покоординатно отступы (margin's) виджета и возвращает результат в виде 'GuiSize'.
getWidgetMarginSize :: MonadIO m => Widget -> m GuiSize
getWidgetMarginSize = fmap marginSize . getWidgetMargin
{-# INLINE getWidgetMarginSize #-}

-- | Возвращает координаты виджета вместе с полями.
getWidgetRectWithMargin :: MonadIO m => Widget -> m GuiRect
getWidgetRectWithMargin widget = do
    w <- readMonadIORef widget
    return $ rectGrowByMargin (widgetMargin w) $ widgetRect w
{-# INLINE getWidgetRectWithMargin #-}

-- | Уменьшить заданный прямоугольник - сдвинуть стороны к центру, на величины оступов из параметров виджета.
-- Если исходный размер прямоугольника по какой либо координате \<0, - прямоугольник не меняется по этой координате.
-- пересчитанный прямоугольник передаётся в @setWidgetRect@ и возвращается этой функцией.
-- Иными словами. К функции передаётся желаемый размер виджета вместе с margin's,
-- он уменьшается (если можно) его на размер отступов заложенных в виджете и устанавливат размер виджета.
setWidgetRectWithMarginShrink :: MonadIO m => Widget -> GuiRect -> m GuiRect
setWidgetRectWithMarginShrink widget newRect =
    do r <- (`rectShrinkByMargin` newRect) <$> getWidgetMargin widget
       setWidgetRect widget r
       return r
{-# INLINE setWidgetRectWithMarginShrink #-}

-- | Рассчитывает, какой размер будет у указаного 2D размера если к нему прибавить поля данного виджета.
-- Поля не прибавляются если исходная координата \<0.
calcWidgetSizeWithMargin :: MonadIO m => Widget -> GuiSize -> m GuiSize
calcWidgetSizeWithMargin widget initSz = do
    mSz <- getWidgetMarginSize widget
--  return $ (\i m -> if i<=0 then i else i+m) <$> initSz <*> mSz
    return $ (\i m -> if i<0 then i else i+m) <$> initSz <*> mSz
{-# INLINE calcWidgetSizeWithMargin #-}

-- | Получает новые координаты виджета с полями, и если координаты изменились, вызывает @onResizing@.
widgetResizingIfChanged :: MonadIO m => Widget -> GuiRect -> m ()
widgetResizingIfChanged widget newRect = do
    oldRect <- getWidgetRectWithMargin widget
    when (newRect /= oldRect) $ do
        fs <- getWidgetFns widget
        onResizing fs widget newRect

-- | Возвращает видимою на экране область в координатах виджета.
getWidgetVisibleRect :: MonadIO m => Widget -> m GuiRect
getWidgetVisibleRect widget = do
    w <- readMonadIORef widget
    return $ SDL.Rectangle (getRectLT $ widgetCanvasRect w) $ sizeOfRect $ widgetRect w

-- | Возвращает видимою на экране область в координатах виджета.
-- Подходит для вызова из обработчика @onDraw@.
getVisibleRect :: MonadIO m => Widget -> Canvas m GuiRect
getVisibleRect = lift . getWidgetVisibleRect
{-# INLINE getVisibleRect #-}

----------------------- ** Функции использующие вектор дочерих виджетов.

-- | Возвращает число дочерних виджетов.
getWidgetChildrenCount :: MonadIO m => Widget -> m Int
getWidgetChildrenCount = fmap ( V.length . cildrenWidgets) . readMonadIORef
{-# INLINE getWidgetChildrenCount #-}

-- | Находит номер виджета если он явяется прямым потомком указанного виджета.
-- Функция принимает запись виджета 'WidgetRecord', а не ссылку на эту запись Widget.
-- Для оптимизации, когда запись по ссылке уже получена.
getChildWidgetIx' :: WidgetRecord -> -- ^ Виджет - предок
                     Widget -> -- ^ Виджет - искомый потомок.
                     Maybe Int
getChildWidgetIx' w whatFound = V.findIndex (whatFound ==) $ cildrenWidgets w
{-# INLINE getChildWidgetIx' #-}

-- | Находит номер виджета если он явяется прямым потомком указанного виджета.
getChildWidgetIx :: MonadIO m =>  Widget -> -- ^ Виджет - предок
                                  Widget ->  -- ^ Виджет - искомый потомок.
                                  m (Maybe Int)
getChildWidgetIx widget whatFound = (`getChildWidgetIx'` whatFound) <$> readMonadIORef widget
{-# INLINE getChildWidgetIx #-}

-- | Возращает виджет-потомок по номеру в векторе виджетов.
getWidgetChild :: MonadIO m => Widget -> Int -> m (Maybe Widget)
getWidgetChild widget ix = ((V.!? ix) . cildrenWidgets) <$> readMonadIORef widget
{-# INLINE getWidgetChild #-}

-- | Переставляет два виджета-потомка местами.
swapChildWidgets :: MonadIO m => Widget -> Int -> Int -> m ()
swapChildWidgets parent l r = modifyMonadIORef' parent $ \w ->
    w{cildrenWidgets= V.modify (\v -> VM.swap v l r) $ cildrenWidgets w}
{-# INLINEABLE swapChildWidgets #-}

-------------------- *** Map-ы по вектору виджетов.

-- | Map по вектору виджетов с результатом - вектором.
mapByWidgetChildren:: MonadIO m => (Widget -> m a) -> Widget -> m (V.Vector a)
mapByWidgetChildren f wRef = V.mapM f =<< (cildrenWidgets <$> readMonadIORef wRef)
{-# INLINE mapByWidgetChildren #-}

-- | Map по вектору виджетов с нумерацией от 0 и результатом - вектором.
imapByWidgetChildren:: MonadIO m => (Int -> Widget -> m a) -> Widget -> m (V.Vector a)
imapByWidgetChildren f wRef = V.imapM f =<< (cildrenWidgets <$> readMonadIORef wRef)
{-# INLINE imapByWidgetChildren #-}

-- | Map по вектору виджетов с игнорированием результатов.
mapByWidgetChildren_:: MonadIO m => (Widget -> m a) -> Widget -> m ()
mapByWidgetChildren_ f wRef = V.mapM_ f =<< (cildrenWidgets <$> readMonadIORef wRef)
{-# INLINE mapByWidgetChildren_ #-}

-- | Map по вектору виджетов с нумерацией от 0 и игнорированием результатов.
imapByWidgetChildren_:: MonadIO m => (Int -> Widget -> m a) -> Widget -> m ()
imapByWidgetChildren_ f wRef = V.imapM_ f =<< (cildrenWidgets <$> readMonadIORef wRef)
{-# INLINE imapByWidgetChildren_ #-}

----------------------  *** Свёртки по вектору виджетов.

-- | Свёртка по вектору виджетов.
foldByWidgetChildren:: MonadIO m => (a -> Widget -> m a) -> a -> Widget -> m a
foldByWidgetChildren f a wRef = V.foldM f a =<< (cildrenWidgets <$> readMonadIORef wRef)
{-# INLINE foldByWidgetChildren #-}

-- | Строгая свёртка по вектору виджетов.
foldByWidgetChildren':: MonadIO m => (a -> Widget -> m a) -> a -> Widget -> m a
foldByWidgetChildren' f a wRef = V.foldM' f a =<< (cildrenWidgets <$> readMonadIORef wRef)
{-# INLINE foldByWidgetChildren' #-}

ifoldByWidgetChildren:: MonadIO m => (a -> Int -> Widget -> m a) -> a -> Widget -> m a
ifoldByWidgetChildren f a wRef = V.ifoldM f a =<< (cildrenWidgets <$> readMonadIORef wRef)
{-# INLINE ifoldByWidgetChildren #-}

-- | Свёртка по вектору виджетов с нумерацией от 0.
ifoldByWidgetChildren':: MonadIO m => (a -> Int -> Widget -> m a) -> a -> Widget -> m a
ifoldByWidgetChildren' f a wRef = V.ifoldM' f a =<< (cildrenWidgets <$> readMonadIORef wRef)
{-# INLINE ifoldByWidgetChildren' #-}

-- | Свёртка по вектору виджетов с игнорированием результата.
foldByWidgetChildren_:: MonadIO m => (a -> Widget -> m a) -> a -> Widget -> m ()
foldByWidgetChildren_ f a wRef = V.foldM_ f a =<< (cildrenWidgets <$> readMonadIORef wRef)
{-# INLINE foldByWidgetChildren_ #-}

-- | Строгая свёртка по вектору виджетов с игнорированием результата.
foldByWidgetChildren'_:: MonadIO m => (a -> Widget -> m a) -> a -> Widget -> m ()
foldByWidgetChildren'_ f a wRef = V.foldM'_ f a =<< (cildrenWidgets <$> readMonadIORef wRef)
{-# INLINE foldByWidgetChildren'_ #-}

-- | Свёртка по вектору виджетов с нумерацией от 0 и с игнорированием результата.
ifoldByWidgetChildren_:: MonadIO m => (a -> Int -> Widget -> m a) -> a -> Widget -> m ()
ifoldByWidgetChildren_ f a wRef = V.ifoldM_ f a =<< (cildrenWidgets <$> readMonadIORef wRef)
{-# INLINE ifoldByWidgetChildren_ #-}

-- | Строгая свёртка по вектору виджетов с нумерацией от 0 и с игнорированием результата.
ifoldByWidgetChildren'_:: MonadIO m => (a -> Int -> Widget -> m a) -> a -> Widget -> m ()
ifoldByWidgetChildren'_ f a wRef = V.ifoldM'_ f a =<< (cildrenWidgets <$> readMonadIORef wRef)
{-# INLINE ifoldByWidgetChildren'_ #-}

--------------------------- ** Функции извлечения информации из родительского виджета.

-- | Определяет номер данного виджета с векторе потомков предка.
getWidgetParentIx :: MonadIO m => Widget -> m (Maybe Int)
getWidgetParentIx widget = getWidgetParent widget >>= (`getChildWidgetIx` widget)
{-# INLINE getWidgetParentIx #-}

-- | Возвращает родительский виджет и его обработчики событий.
getWidgetParentFns:: MonadIO m => Widget -> m (Widget,WidgetFunctions)
getWidgetParentFns = (\p -> (p,) <$> getWidgetFns p) <=< getWidgetParent
{-# INLINE getWidgetParentFns #-}

-- | Найти предыдущий и следующий виджет из того же вектора дочерних виджетов что и данный.
-- вектор не просматривается по кругу. Если текущий виджет в начале вектора, то
-- возвращается (Nothing, ...), если в конце (...,Nothing)
getPrevNextWidgets :: MonadIO m => Widget -> m (Maybe Widget,Maybe Widget)
getPrevNextWidgets widget = do  v <- (fmap cildrenWidgets . readMonadIORef) =<< getWidgetParent widget
                                return (case V.findIndex (widget ==) v of
                                    Just i -> (v V.!? (i - 1),v V.!? (i + 1))
                                    _ -> (Nothing,Nothing))
{-# INLINE getPrevNextWidgets #-}

-------------------------- ** Оперции с деревом виджетов.

-- | Выполнить действие для всех виджетов дерева начиная с заданного виджета.
forEachWidgets :: MonadIO m => Widget -> (Widget -> m ()) -> m ()
forEachWidgets startWidget f = go startWidget
    where go widget = f widget >> mapByWidgetChildren_ go widget
{-# INLINE forEachWidgets #-}

-- | Проверка - является данный виджет корневым (@mainWidget@ или @winFgWidget@ окна).
isMainWidget:: MonadIO m =>  Widget -> m Bool
isMainWidget widget = ((widget==).parentWidget) <$> readMonadIORef widget
{-# INLINE isMainWidget #-}

-- | Для любого виджета найти его корневой виджет.
getWinMainWidget:: MonadIO m => Widget -> m Widget
getWinMainWidget w = do p <- getWidgetParent w
                        if p == w then return p else getWinMainWidget p
-- Не годится для дерева Forward виджетов : getWindowMainWidget <=< getWidgetWindow

-- | Найти виджет в дереве двигаясь по дереву вперёд с обходом всех подветок покругу пока или
-- предикат не вернёт True, или не вернёмся к исходному виджету.
findWidgetInTreeForward :: MonadIO m => (Widget -> WidgetFlags -> m Bool) -> Widget -> m Widget
findWidgetInTreeForward predicate startWidget = do
    startParent <- getWidgetParent startWidget
    startStruct <- readMonadIORef startParent
    rootWidget  <- getWinMainWidget startWidget
    let go w s ix needToRoot =
            case cildrenWidgets s V.!? ix of
                Just w' | w' == startWidget -> return $ Just w'
                        | otherwise -> do
                            s' <- readMonadIORef w'
                            b <- predicate w' (widgetFlags s')
                            if b then return $ Just w'
                            else do r <- go w' s' 0 False
                                    case r of
                                        Nothing -> go w s (ix+1) needToRoot
                                        res -> return res
                _ | needToRoot ->
                        let parent = parentWidget s in
                        if rootWidget == parent then go w s 0 False
                        else do s' <- readMonadIORef parent
                                case getChildWidgetIx' s' w of
                                    Just i -> go parent s' (i+1) True
                                    _ -> error "findWidgetInTreeForward : widget not found in child vector"
                  | otherwise -> return Nothing
        startIx = case getChildWidgetIx' startStruct startWidget of
                        Just i -> i+1
                        _ -> error "findWidgetInTreeForward : start widget not found in child vector"
    r <- go startParent startStruct startIx True
    case r of
        Nothing -> error "findWidgetInTreeForward : widget not found in widget's tree"
        Just res -> return res

-- | Найти виджет в дереве двигаясь по дереву назад с обходом всех подветок покругу пока или
-- предикат не вернёт True, или не вернёмся к исходному виджету.
findWidgetInTreeBackward :: MonadIO m => (Widget -> WidgetFlags -> m Bool) -> Widget -> m Widget
findWidgetInTreeBackward predicate startWidget = do
    startParent <- getWidgetParent startWidget
    startStruct <- readMonadIORef startParent
    rootWidget  <- getWinMainWidget startWidget
    let lastIx s = V.length (cildrenWidgets s) - 1
        go w s (-1) True =
                        let parent = parentWidget s in
                        if rootWidget == parent then go w s (lastIx s) False
                        else do s' <- readMonadIORef parent
                                case getChildWidgetIx' s' w of
                                    Just i -> go parent s' (i-1) True
                                    _ -> error "findWidgetInTreeBackward : widget not found in child vector"
        go _ _ (-1) _ = return Nothing
        go w s ix needToRoot =
            let w' = cildrenWidgets s `V.unsafeIndex` ix in
            if w' == startWidget then return $ Just w'
            else do
                            s' <- readMonadIORef w'
                            b <- predicate w' (widgetFlags s')
                            if b then return $ Just w'
                            else do r <- go w' s' (lastIx s') False
                                    case r of
                                        Nothing -> go w s (ix-1) needToRoot
                                        res -> return res
        startIx = case getChildWidgetIx' startStruct startWidget of
                        Just i -> i-1
                        _ -> error "findWidgetInTreeBackward : start widget not found in child vector"
    r <- go startParent startStruct startIx True
    case r of
        Nothing -> error "findWidgetInTreeBackward : widget not found in widget's tree"
        Just res -> return res

-- | Предикат для использования в @findNextTabbedWidget@, @findPrevTabbedWidget@.
isTabbedInternalPredicate :: MonadIO m => Widget -> WidgetFlags -> m Bool
isTabbedInternalPredicate _ fl = let f = WidgetEnable .|. WidgetVisible .|. WidgetFocusable .|. WidgetTabbed
                in return $ (fl .&. f) == f
{-# INLINE isTabbedInternalPredicate #-}

-- | Найти следующий виджет на который можно перейти табуляцией или вернуть исходный виджет,
-- если других подходящий нет.
findNextTabbedWidget :: MonadIO m => Widget -> m Widget
findNextTabbedWidget = findWidgetInTreeForward isTabbedInternalPredicate
{-# INLINE findNextTabbedWidget #-}

-- | Найти следующий виджет на который можно перейти обратной табуляцией (Shift-Tab) или
-- вернуть исходный виджет, если других подходящий нет.
findPrevTabbedWidget :: MonadIO m => Widget -> m Widget
findPrevTabbedWidget = findWidgetInTreeBackward isTabbedInternalPredicate
{-# INLINE findPrevTabbedWidget #-}

--------------------- ** Доступ к данным 'Window' или 'Gui' через виджет.

-- | Получить 'Gui' из виджета.
getGuiFromWidget:: MonadIO m => Widget -> m Gui
getGuiFromWidget = getWindowGui <=< getWidgetWindow
{-# INLINE getGuiFromWidget #-}

-- | Получить активный 'Skin' из виджета.
getSkinFromWidget:: MonadIO m => Widget -> m Skin
getSkinFromWidget = getSkinFromWin <=< getWidgetWindow
{-# INLINE getSkinFromWidget #-}

----------------------- ** Использование виджета базового уровня

-- | Пометить виджет для перерисовки в следующий раз.
-- Кроме виджета, помечается для перерисовки его окно.
-- До реальной перерисовки можно пометить много раз.
markWidgetForRedraw :: MonadIO m => Widget -> m ()
markWidgetForRedraw widget = do
    w <- readMonadIORef widget
    unless (isWidgetMarkedForRedrawing' w) $ do
        logOnErrInWidget widget "markWidgetForRedraw.onMarkForRedrawNotiy" $
            onMarkForRedrawNotiy (widgetFns w) widget
        widgetFlagsAdd widget WidgetRedrawFlag
        windowFlagsAdd (windowOfWidget w) WindowRedrawFlag
{-# INLINE markWidgetForRedraw #-}

------------------------- *** Изменение координат виджета.

-- | Переместить видимую область виджета @widgetRect@ (левую верхнююточку) в заданную точку, в координатах родителя.
-- Без полей (margin's).
-- Если размер изменился, вызывается @markWidgetForRedraw@.
-- Никакие обработчики событий, включая @onResizing@ не вызываются. Их можно вызвать отдельно.
moveWidget :: MonadIO m => Widget -> GuiPoint -> m ()
moveWidget widget p = do
    w <- readMonadIORef widget
    let (SDL.Rectangle pO sz) = widgetRect w
    when (pO /= p) $ do
        writeMonadIORef widget w{widgetRect=SDL.Rectangle p sz}
        markWidgetForRedraw widget

-- | Устанавливает новый размер видимой области виджета в @widgetRect@.
-- Без полей (margin's).
-- Если размер изменился, вызывается @markWidgetForRedraw@.
-- Никакие обработчики событий, включая @onResizing@ не вызываются. Их можно вызвать отдельно.
resizeWidget :: MonadIO m => Widget -> GuiSize -> m ()
resizeWidget widget sz = do
    w <- readMonadIORef widget
    let (SDL.Rectangle p szO) = widgetRect w
    when (szO /= sz) $ do
        writeMonadIORef widget w{widgetRect=SDL.Rectangle p sz}
        markWidgetForRedraw widget

-- | Устанавливает новый размер видимой области виджета в @widgetRect@ и размер @widgetCanvasRect@.
-- Без полей (margin's).
-- Если размер изменился, вызывается @markWidgetForRedraw@.
-- Никакие обработчики событий, включая @onResizing@ не вызываются. Их можно вызвать отдельно.
resizeWidgetWithCanvas :: MonadIO m => Widget -> GuiSize -> m ()
resizeWidgetWithCanvas widget sz = do
    w <- readMonadIORef widget
    let (SDL.Rectangle p szO) = widgetRect w
    when (szO /= sz) $ do
        writeMonadIORef widget w{widgetRect=SDL.Rectangle p sz
                                ,widgetCanvasRect=SDL.Rectangle (getRectLT $ widgetCanvasRect w) sz}
        markWidgetForRedraw widget

-- | Вычисляет смещение левой верхней точки видимой области виджета (без учёта полей - margin's)
-- относительно клиентской области окна.
getWidgetCoordOffset:: MonadIO m => Widget -> m GuiCoordOffset
getWidgetCoordOffset = go zero
    where go sumOff widget = do
            w <- readMonadIORef widget
            let (SDL.Rectangle widgP _) = widgetRect w
                pInWinCoord = widgP .+^ sumOff
                off = pInWinCoord .-. getRectLT (widgetCanvasRect w)
                parent = parentWidget w
            if widget == parent then return off else go off parent

-- No exported.
-- | Передаёт границы без изменения к @onSizeChangedParentNotify@ родителя пометив его для перерисовки.
notifyParentOnSizeChangedAndMarkForRedraw :: MonadIO m => Widget -> GuiSize -> m ()
notifyParentOnSizeChangedAndMarkForRedraw widget sz = do
    (parent,fs) <- getWidgetParentFns widget
    markWidgetForRedraw parent
    logOnErrInWidget widget "notifyParentOnSizeChangedAndMarkForRedraw.onSizeChangedParentNotify" $
        onSizeChangedParentNotify fs parent widget sz

-- | Добавляет к указанному размеру поля (подробнее, см. @calcWidgetSizeWithMargin@) и
-- переаёт @notifyParentOnSizeChangedAndMarkForRedraw@.
-- В общем, получает размер без полей, прибавляет поля если размер не отрицательный и вызывает
-- @onSizeChangedParentNotify@ родителя пометив его для перерисовки.
-- Часто, но не всегда и не только вызывается из @onCreate@.
notifyParentAboutSize :: MonadIO m => Widget -> GuiSize -> m ()
notifyParentAboutSize widget initSz = calcWidgetSizeWithMargin widget initSz >>=
            notifyParentOnSizeChangedAndMarkForRedraw widget
{-# INLINE notifyParentAboutSize #-}

-- No exported.
-- |
simpleOnResizing' :: MonadIO m => (Widget -> GuiRect -> m GuiRect)
                                    -> Widget -> GuiRect -> m GuiRect
simpleOnResizing' recalcWithMargin widget newRect = do
    r@(SDL.Rectangle _ sz) <- recalcWithMargin widget newRect
    getWidgetParent widget >>=  markWidgetForRedraw
    setWidgetCanvasRect widget $ SDL.Rectangle zero sz
    return r


-- | Чаще всего вызывается из @onResizing@. Получает размеры с полями (margin's),
-- уменьшает прямоугольник на размер полей, устанавливает таким осносной прямоугольник виджета - @setWidgetRect@.
-- Так же устанавливает @setWidgetCanvasRect  $ SDL.Rectangle zero sz @ в тот же размер.
-- Возвращает уменьшенный прямоугольник (установленный в @setWidgetRect@).
simpleOnResizing :: MonadIO m => Widget -> GuiRect -> m GuiRect
simpleOnResizing = simpleOnResizing' setWidgetRectWithMarginShrink
{-# INLINE simpleOnResizing #-}

-- | Как @simpleOnResizing@, но ещё дан начальный размер. При его отрицательных координатах
-- они (оставаясь отрицательными) заменяют имеющиеся координаты прямоугольника без полей и
-- передаются в @notifyParentOnSizeChangedAndMarkForRedraw@ для вызова
-- @onSizeChangedParentNotify@ родителя пометив его для перерисовки.
-- Нужна когда рлдитель-контейнер поддерживает отрицательные размеры для обозначения требования
-- предоставления всего оставшегося поля под виджет.
extendableOnResizing :: MonadIO m => GuiSize -> Widget -> GuiRect -> m GuiRect
extendableOnResizing initSz@(V2 initW initH) widget newRect@(SDL.Rectangle _ sz) = do
    r <- simpleOnResizing widget newRect
    when (initW<0 || initH<0) $
        notifyParentOnSizeChangedAndMarkForRedraw widget $ sizeRestoreNegative initSz sz
    return r
{-# INLINE extendableOnResizing #-}

------------------------------ *** Поиск и преобразование из клиентских координат в координаты виджета.

-- | Пытается найти виджет по координатам клиентской области начиная с заданного виджета.
-- Для использования внутри /GUI.BaseLayer/.
coordToWidget:: MonadIO m =>
    Widget -> -- ^ Начальный для поиска виджет в дереве. Обычно корневой.
    GuiPoint -> -- ^ Точка в координатах клиентской области окна.
    -- | Если виджет найден, то (виджет, координата пересчитанная в координаты виджета).
    m (Maybe (Widget,GuiCoordOffset))
coordToWidget widget' pnt = do
        r <- getWidgetRect widget'
        go zero r widget'
    where go parentOff clipRect widget = do
            w <- readMonadIORef widget
            let (SDL.Rectangle widgP widgSz) = widgetRect w
                pInWinCoord = widgP .+^ parentOff
                rect = rectIntersection clipRect $ SDL.Rectangle pInWinCoord widgSz
            if ((widgetFlags w .&. WidgetVisible) /= WidgetNoFlags) && isInRect rect pnt then
                let off = pInWinCoord .-. getRectLT (widgetCanvasRect w)
                    byChilds 0 = return $ Just (widget,off)
                    byChilds i = do
                        let ii = pred i
                        r <- go off rect $ V.unsafeIndex (cildrenWidgets w) ii
                        case r of
                            j@(Just _) -> return j
                            _ -> byChilds ii
                in byChilds $ V.length $ cildrenWidgets w
            else
                return Nothing

-- | Пытается найти по координатам клиентской области вначале виджет из Foreground дерева виджетов
-- (всплывающие подсказки и подобное), а если не найден там, то начиная с основного дерева виджетов окна.
-- Для использования внутри /GUI.BaseLayer/.
mouseToWidget:: MonadIO m => Window -> GuiPoint -> m (Maybe (Widget,GuiCoordOffset))
mouseToWidget rfWin pnt = do
    fgWidget <- getWindowForegroundWidget rfWin
    mbFg <- coordToWidget fgWidget pnt
    let forMain = getWindowMainWidget rfWin >>= (`coordToWidget` pnt)
    case mbFg of
        Just (w,_) | w == fgWidget -> forMain
                   | otherwise -> return mbFg
        _ -> forMain
{-# INLINEABLE mouseToWidget #-}

--------------------- ** Обработка прерываний.

logPutLnWidget  :: MonadIO m => Widget -> -- ^ Ссылка на виджет.
                                TS.Builder -> -- ^ Логируемый текст.
                                m ()
logPutLnWidget widget t = (`L.logPutLn` t) =<< guiGetLog =<< getWindowGui =<< getWidgetWindow widget

-- | Перехват и логирование прерываний в контексте виджета.
logOnErrInWidget' :: MonadIO m => Widget -> -- ^ Ссылка на виджет.
                                  TS.Builder -> -- ^ Префикс сообщения об исключении, если оно возникнет.
                                  IO a -> -- ^ Действие выполняемое в случае возникновение исключения.
                                  IO a -> -- ^ Действие в котором может возникнуть неперехваченное исключение.
                                  m a
logOnErrInWidget' widget t h f = liftIO $ guiCatch f (\e -> do
        l <- guiGetLog =<< getWindowGui =<< getWidgetWindow widget
        L.logOnSomeException l t e >> h)
{-# INLINEABLE logOnErrInWidget' #-}

-- | Перехват и логирование прерываний в контексте виджета.
logOnErrInWidget :: MonadIO m => Widget -> -- ^ Ссылка на виджет.
                                 TS.Builder -> -- ^ Префикс сообщения об исключении, если оно возникнет.
                                 IO () -> -- ^ Действие в котором может возникнуть неперехваченное исключение.
                                 m ()
logOnErrInWidget widget t = logOnErrInWidget' widget t (return ())
{-# INLINEABLE logOnErrInWidget #-}

--------------------- ** Отладочные функции вывода парметров виджета(ов) в виде строки.

widgetCoordsToStr:: MonadIO m => Widget -> m String
widgetCoordsToStr widget = do
    rect <- getWidgetRect widget
    canv <- getWidgetCanvasRect widget
    marg <- getWidgetMargin widget
    return $ concat ["rect=", rectToBriefStr rect, "  canv=", rectToBriefStr canv,
     "  marg=", marginToBriefStr marg ]

showWidgets :: MonadIO m => Widget -> Maybe Widget -> m String
showWidgets widget markedWidget = do
    sc <- foldByWidgetChildren' (\s w -> do { n <- showWidgets w markedWidget; return $ concat [s,'{':n,"}"]})
        "  chlds=[" widget
    s <- widgetCoordsToStr widget
    return $ concat [if Just widget == markedWidget then "@" else "",s,sc,"]"]

showWidgetsFromMain :: MonadIO m => Widget -> m String
showWidgetsFromMain widget = do
    mainWidg <- getWinMainWidget widget
    showWidgets mainWidg $ Just widget

-- | Для отладки. Вывести дерево виджетов. Пометить указанный виджет.
showWinWidgets :: MonadIO m => Window -> Maybe Widget -> m String
showWinWidgets rfWin markedWidget = getWindowMainWidget rfWin >>= (`showWidgets` markedWidget)

--------------------- * Виджет пользовательского уровня.

-- | Пользовательский виджет. Сочетает виджет базового уровня и данные конкретного виджета.
data GuiWidget a = GuiWidget { baseWidget :: Widget
                             , widgetData :: a
                             }

-- | Устанавливает (True) или сбрасывает (False) указанный флаг.
-- Если флаг на самом деле изменился, вызывается @markWidgetForRedraw@.
-- Виджет указывается как 'GuiWidget' .т.е. виджет верхнего уровня.
setWidgetFlag :: MonadIO m => WidgetFlags -> GuiWidget a -> Bool -> m ()
setWidgetFlag fl g ena = let widget = baseWidget g in do
    o <- allWidgetFlags widget fl
    if ena then
        unless o $ do
            widgetFlagsAdd widget fl
            markWidgetForRedraw widget
    else when o $ do
            widgetFlagsRemove widget fl
            markWidgetForRedraw widget

-- | Устанавливает enable/disable состояние виджета верхнего уровня.
enableWidget :: MonadIO m => GuiWidget a -> Bool -> m ()
enableWidget = setWidgetFlag WidgetEnable
{-# INLINE enableWidget #-}

-- | Устанавливает состояние видимости  виджета верхнего уровня.
visibleWidget :: MonadIO m => GuiWidget a -> Bool -> m ()
visibleWidget = setWidgetFlag WidgetVisible
{-# INLINE visibleWidget #-}

-- | Если виджет (частично) прозрачен и изменяет свой вид в зависимости от положения мыши, то,
-- для правильной отрисовки, после его создания следует выполнить эту функциию.
fnsCorrectionForTransparent :: MonadIO m => GuiWidget _a -> m ()
fnsCorrectionForTransparent gw = do
    let widget' = baseWidget gw
    fns <- getWidgetFns widget'
    parent <- getWidgetParent widget'
    setWidgetFns widget' fns{
                onGainedMouseFocus = \widget pnt -> onGainedMouseFocus fns widget pnt >>
                                                    markWidgetForRedraw parent
                ,onLostMouseFocus = \widget -> onLostMouseFocus fns widget >> markWidgetForRedraw parent
                            }

--------------------- ** Простейший виджет пользовательского уровня не имеюший своих параметров.

-- | Тип для простейшего виджета без параметров.
data SimpleWidget = SimpleWidget

-- | Конструирование простейшего виджета без параметров. C одним флагом WidgetVisible.
-- Обычно создаётся в демонстрационных целях или для изучения пакета.
mkSimpleWidget:: MonadIO m => WidgetMargin ->  -- ^ Поля виджета.
                              Widget ->  -- ^ Родительский виджет
                              WidgetFunctions -> -- ^ запись с функциями - обработчиками событий базового уровня.
                              m (GuiWidget SimpleWidget)
mkSimpleWidget marg = mkWidget WidgetVisible marg SimpleWidget

--------------------- * Создание виджетов.

-- | Создаёт ссылку на виджет базового уровня с полями из аргументов и по умолчанию.
-- Виджет ещё не включен в состав дерева виджетов.
-- Используется в функции @GUI.BaseLayer.Core.mkWidget@ и при создании корневых виджетов.
-- Не использовать за пределами /GUI.BaseLayer/.
mkWidget':: MonadIO m => Window -> -- ^ Окно виджета
                         WidgetFlags ->  -- ^ Флаги виджета.
                         WidgetMargin ->  -- ^ Поля виджета.
                         Widget ->  -- ^ Родительский виджет
                         WidgetFunctions ->  -- ^ запись с функциями - обработчиками событий
                                             -- базового уровня.
                         m Widget
mkWidget' win fl marg parent fs =
        newMonadIORef WidgetRecord { windowOfWidget = win
                             , parentWidget = parent
                             , cildrenWidgets = V.empty
                             , widgetRect = SDL.Rectangle zero zero
                             , widgetCanvasRect = SDL.Rectangle zero zero
                             , widgetMargin = marginToLTRB marg
                             , widgetFlags = if (fl .&. WidgetVisible)  == WidgetVisible then
                                                fl .|. WidgetRedrawFlag
                                             else fl
                             , widgetCursor = DefCursorIx
                             , widgetFns = fs
                             }

-- | Эта функция используется в функциях создания конкретных, пользовательских, виджетов выше /GUI.BaseLayer/.
mkWidget:: MonadIO m => WidgetFlags -> -- ^ Флаги виджета.
                        WidgetMargin -> -- ^ Поля виджета.
                        a -> -- ^ Данные виджета верхнего уровня.
                        Widget ->  -- ^ Родительский виджет
                        WidgetFunctions -> -- ^ запись с функциями - обработчиками событий
                                           -- базового уровня.
                        m (GuiWidget a)
mkWidget fl marg a parent fs = do
        win   <- getWidgetWindow parent
        child <- mkWidget' win fl marg parent fs
        modifyMonadIORef' parent (\w -> w{cildrenWidgets= cildrenWidgets w `V.snoc` child})
        logOnErrInWidget child "mkWidget.onCreate" $ onCreate fs child
        return $ GuiWidget child a

-- | Эта функция используется в виджетах-контейнерах. Через неё добавляется дочерний виджет.
-- Функция создающая пользовательский виджет передаётся вторым аргументом.
-- Обычно вызывается изнутри ($+).
createWidget:: MonadIO m => Widget -> (Widget -> Skin -> m (GuiWidget a)) -> m (GuiWidget a)
createWidget parent initF = getSkinFromWidget parent >>= initF parent
{-# INLINEABLE createWidget #-}
