{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
-- |
-- Module:      GUI.Widget.HorizontalItems
-- Copyright:   (c) 2017-2020 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Поле с некими элементами заданными контейнером и располагаемые горизонтально с
-- возможностью прокрутки.
-- Это виджет для весьма общего случая. Более конкретные виджеты - для отобращения цепочек линков, директорий в пути,
-- закладок или панели кнопок изготовляются из этого виджета путём определенния типов, их @instance@ для
-- ViewableItems, ViewableVarWidthItems, HorizontalSeparator и обработчиков событий этого виджета.
-- Опционально поддерживается перетаскивание элементов, дополнительная кнопка
-- (например, "Закрыть" или "Создать новую директориюя" в производных виджетах).
-- Отображаемые данные хранятся по ссылке. Для их обновления нужно передать новые через setValue.

module GUI.Widget.HorizontalItems(
    -- * Флаги @horizItems@.
    pattern OptBtnIx
    -- * Классы и типы используемые с @horizItems@.
    ,NeighborSwap(..),HorizItsDef(..),HorizItsData
    -- * Функции.
    ,horizItems
    ) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU
import Data.Bits
import Data.IORef
import qualified SDL
import SDL.Vect
import Control.Monad.Extra (whenJust)
import Data.Default
import GUI
import GUI.Widget.Handlers
import GUI.Utils.ViewableItems
import qualified Data.Vector.Unboxed.Utils as VU

pattern OptBtnIx :: Int; pattern OptBtnIx = -1
pattern PaddingX :: Coord; pattern PaddingX = 5
pattern BorderThickness :: Coord; pattern BorderThickness = 1

-- | Начальные настройки виджета.
data HorizItsDef = HorizItsDef {
          horizItsFormItemDef  :: FormItemWidgetDef -- ^ Общие настройки для всех виджетов для форм,
                                                    -- в настоящий момент только margin's.
        , horizItsWidth   :: Coord -- ^ Ширина поля редактирования. Высота определяется как максимальная из
                                   -- возвращаемых @viewablePrepare@ и @horizSepPrepare@
                                   -- и внутренними полями фиксированного размера.
        , horizItsFlags   :: WidgetFlags -- ^ Флаги базового виджета.
        , horizItsOptBtn  :: T.Text -- ^ Имя файла из ресурсов для дополнительной (самой правой) кнопки.
                                    --   Если пустая строка, то дополнительной кнопки нет.
        , horizItsMoveOnUpdate :: MoveOnUpdate -- ^  Как перемещать видимое окно списка при обновлении данных.
        , horizItsPermutable :: Bool -- ^ Допустима перестановка элементов мышью.
                               }

instance Default HorizItsDef where
    def = HorizItsDef { horizItsFormItemDef = def
                      , horizItsWidth = 100
                      , horizItsFlags = WidgetVisible .|. WidgetEnable
                      , horizItsOptBtn = T.empty
                      , horizItsMoveOnUpdate = CurVisibleOnUpdate
                      , horizItsPermutable = False
                      }

-- | Не экспортируемый тип записи. Хранится по ссылке в 'HorizItsData'.
data Handlers a p = Handlers    { hndlrOnMove :: forall m. MonadIO m => Int -> m ()
                                , hndlrOnClk :: forall m. MonadIO m => Int -> m ()
                                , hndlrOnDblClk :: forall m. MonadIO m => Int -> m ()
                                , hndlrOnRightClick :: forall m. MonadIO m => Int -> m ()
                                , hndlrDataUpdate :: forall m. (MonadIO m, ViewableVarWidthItems a p) =>
                                        Widget -> a -> m ()
                                , hndlrIxUpdate :: forall m. MonadIO m => Widget -> Int -> m ()
                                , doNeighborSwap :: forall m. (MonadIO m, ViewableVarWidthItems a p) =>
                                        Int -> a -> m a
                                }

-- | Тип созданного виджета. Обычно используется как  @GuiWidget HorizItsData@.
data HorizItsData a p = HorizItsData    { refData :: IORef a
                                        , refHandlers :: IORef (Handlers a p)
                                        , refCurIx :: IORef Int
                                        }

class NeighborSwap w c | w -> c where
    -- | Установка функции для перестановки соседних элементов, когда они перемещаются мышью.
    --   Необходимо установить если такое премещение предусматривается.
    -- @horizItems@, в котором устанавливаемя функция обработчик используется,  использует данные в режиме read only.
    -- Для обновления нужно установить свой обработчик с помощью @setNeighborSwap@.
    setNeighborSwap :: MonadIO m => w -> (
        -- | Функция которая будет вызываться для перестановки элементов
        forall n. MonadIO n => Int -> -- ^ Левый из переставляемых элементов.
                                c -> -- ^ исходные данные
                                n c
                                ) -> m ()

instance ViewableVarWidthItems c p => NeighborSwap (GuiWidget (HorizItsData c p)) c where
    setNeighborSwap w a = modifyMonadIORef' (refHandlers $ widgetData w) (\d -> d{doNeighborSwap= a})

-- | Установка и извлечение номера текущего (выделенного) элемента.
instance ViewableVarWidthItems a p => IxProperty (GuiWidget (HorizItsData a p)) where
    setIx w v = do
        let widget = baseWidget w
            HorizItsData{..} = widgetData w
        ix <- readMonadIORef refCurIx
        counts <- viewableGetCount =<< readMonadIORef refData
        when ( (v /= ix) && (v>=0) && (v < counts)) $
            readMonadIORef refHandlers >>= (\h -> hndlrIxUpdate h widget v)
    getIx w = readMonadIORef (refCurIx $ widgetData w)

-- | Установка функции-обработчика изменение текущей позиции (номера выбранного элемента).
-- Вызывается перед __/(Dbl)Clk/__ если номер меняется, а так же при изменении номера текущего элемента
-- через setIx.
instance Moveable (GuiWidget (HorizItsData a p)) Int where
    onMove w a = modifyMonadIORef' (refHandlers $ widgetData w) (\d -> d{hndlrOnMove= a})

-- | Установка функции-обработчика одинарного щелчка.
-- Значение аргумента @OptBtnIx@ означает что дейтвие выполнено для опциональной кнопки,
-- иначе для указанного номера элемента.
instance Clickable1 (GuiWidget (HorizItsData a p)) Int where
    onClick1 w a = modifyMonadIORef' (refHandlers $ widgetData w) (\d -> d{hndlrOnClk= a})

-- | Установка функции-обработчика двойного щелчка.
-- Значение аргумента @OptBtnIx@ означает что дейтвие выполнено для опциональной кнопки,
-- иначе для указанного номера элемента.
instance DoubleClickable1 (GuiWidget (HorizItsData a p)) Int where
    onDoubleClick1 w a = modifyMonadIORef' (refHandlers $ widgetData w) (\d -> d{hndlrOnDblClk= a})

-- | Установка функции-обработчика одинарного щелчка правой кнопкой указателя.
-- Значение аргумента @OptBtnIx@ означает что дейтвие выполнено для опциональной кнопки,
-- иначе для указанного номера элемента.
instance RightClickable1 (GuiWidget (HorizItsData a p)) Int where
    onRightClick1 w a = modifyMonadIORef' (refHandlers $ widgetData w) (\d -> d{hndlrOnRightClick= a})

instance (ViewableVarWidthItems a p, Eq a) => ValueProperty (GuiWidget (HorizItsData a p)) a where
    setValue w v = do
        let widget = baseWidget w
            HorizItsData{..} = widgetData w
        oldDat <- readMonadIORef refData
        when (v /= oldDat) $ do h <- readMonadIORef refHandlers
                                hndlrDataUpdate h widget v

    getValue w = readMonadIORef (refData $ widgetData w)

-- | Internal for @horizItems@
data IntrnState = IntrnState {
          intrnOff :: Int -- ^ Первый отображаемый элемент (Смещение начала отображения по горизонтали в элементах).
        , intrnWidths :: VU.Vector Coord -- ^ ширины элементов.
                             }

-- | Не экспортируемый тип. Определяет в какой области виджета находится указатель.
data AreaOfMouse = None -- ^ В этом месте не предусмотренно действий.
                 | ItemNo Int -- ^ Указатель на элементе с указанным номером.
                 | BtnLeft | BtnRight | BtnOpt -- ^ На кнопках прокрутки.
                 deriving Eq

-- | Поле с некими элементами заданными контейнером и располагаемые горизонтально с возможностью прокрутки.
horizItems :: (MonadIO m, ViewableVarWidthItems a p, HorizontalSeparator b _q, Eq a) =>
                         HorizItsDef -> -- ^ Параметры виджета.
                         b -> -- ^ Данные для отображения разделителей.
                         a -> -- ^ Исходные отображаемые данные.
                         Widget -> -- ^ Будущий предок в дереве виджетов.
                         Skin -> -- ^ Skin.
                         m (GuiWidget (HorizItsData a p))
horizItems HorizItsDef{..} separatorsData initData parent skin = do
    gui <- getGuiFromWidget parent
    let bkClr = decoreBkColor $ formDecore skin
        lightColor = brdr3DLightColor $ brdr3DColors skin
        darkColor  = brdr3DDarkColor  $ brdr3DColors skin
    (internH',dataPrep,HorizSepPrepare{..},arrWidth,arrTexture,V2 optW optH,mbOptTexture)
        <- runProxyCanvas parent $ do
            (datH,p) <- viewablePrepare gui skin initData
            sepP <- horizSepPrepare gui skin separatorsData
            arrT <- getTexture "ArrBtns.png"
            arrW <- ((`div` 2).xV2) <$> getTextureSize arrT
            (oSz,oT) <- if T.null horizItsOptBtn then return (zero,Nothing)
                        else do t <- getTexture horizItsOptBtn
                                sz <- getTextureSize t
                                return (sz,Just t)
            return (max datH $ max (horizSepPrHeight sepP) $ max arrW $ xV2 oSz,
                    p,sepP,arrW,arrT,oSz,oT)
    refD <- newMonadIORef initData
    refH <- newMonadIORef Handlers { hndlrOnMove = \_ -> return ()
                                   , hndlrOnClk  = \_ -> return ()
                                   , hndlrOnDblClk  = \_ -> return ()
                                   , hndlrOnRightClick = \_ -> return ()
                                   , hndlrDataUpdate = \_ _ -> return () -- tmp. reset later
                                   , hndlrIxUpdate  = \_ _ -> return () -- tmp. reset later
                                   , doNeighborSwap = \_ a -> return a
                                   }
    refC <- newMonadIORef 0 -- Номер текущего элемента
    refM <- newMonadIORef None -- Область, в которой была нажата кнопка мыши (и ещё не отпущена), иначе None.
    refMS <- newMonadIORef None -- То же, но для перестановки элементов мышью. Не изм-ся при проходе разделителя.
    refS <- newMonadIORef IntrnState { intrnOff = 0
                                     , intrnWidths = VU.empty
                                     }
    let fns = noChildrenFns $ V2 horizItsWidth (internH'+2*BorderThickness)

        mkWidthsVector v = do
            count <- viewableGetCount v
            runProxyCanvas parent $ VU.generateM count (viewableGetWidth gui dataPrep v)

        scrollState width IntrnState{..} =
            let leftBtEna = intrnOff > 0
                internW = width - 2*PaddingX - optW
                w = internW - if optW>0 then PaddingX else 0 -- Основное поле без кнопок влево, вправо.
                w' = internW - arrWidth*2 - PaddingX -- Основное поле с кнопками влево, вправо.
                count = VU.length intrnWidths
                rightBtEna = VU.sum (VU.slice intrnOff (count - intrnOff) intrnWidths) +
                      leftSepW intrnOff +
                      (count - intrnOff - 1)*horizSepPrMiddleWidth + horizSepPrLastWidth
                      > if leftBtEna then w' else w
            in (leftBtEna,rightBtEna,if leftBtEna || rightBtEna then w' else w)

        leftSepW i = if i==0 then horizSepPrFirstWidth else horizSepPrMiddleWidth

        -- | Какое минимальное смещение можно установить что бы элемент rightIx ещё не ушёл за правую границу.
        calcScrollRightOff v -- ^ вектор ширин элементов
                           width -- ^ скорректированная цирина поля вывода, с учётом наличия кнопок справа.
                           rightIx =
            let go i rest | i < 0 = 0
                          | otherwise = let rest' = rest - leftSepW i - v VU.! i in
                                            if rest' < 0 then min rightIx $ i+1 else go (i-1) rest'
            in go rightIx $ width - if rightIx == (VU.length v - 1) then horizSepPrLastWidth else horizSepPrMiddleWidth

        -- | Расчитать смещение, так, что бы текущий элемент @curIx@ был виден.
        calcOffForCurVisible width  -- ^ скорректированная цирина поля вывода, с учётом наличия кнопок справа.
                             IntrnState{..} curIx =
            if intrnOff > curIx then curIx else max intrnOff $ calcScrollRightOff intrnWidths width curIx

        coord2ix width state@IntrnState{..} xPos =
            let (leftBtEna,rightBtEna,mainW) = scrollState width state
                xp = xPos - PaddingX
                internW = width - 2*PaddingX
                count = VU.length intrnWidths
            in if | xp < 0 || xp >= internW -> None
                  | optW > 0 && xp >= (internW-optW) -> BtnOpt
                  | xp > (internW - arrWidth*2 - optW) ->
                        if xp < (internW - arrWidth - optW) then
                            if leftBtEna then BtnLeft else None
                        else if rightBtEna then BtnRight else None
                  | otherwise ->
                        let go i x | (i >= count) || (x >= mainW) = None
                                   | otherwise =
                                        let x' = x + leftSepW i in
                                        if xp < x' then None
                                        else let x'' = x' + intrnWidths VU.! i in
                                             if xp < x'' then ItemNo i else go (i+1) x''
                        in go intrnOff 0

        coord2ixM widget xPos = do
            (SDL.Rectangle (P (V2 l _)) (V2 width _)) <- getWidgetRect widget
            state <- readMonadIORef refS
            return $ coord2ix width state $ xPos - l

        writeMS m@(ItemNo _) = writeMonadIORef refMS m
        writeMS _ = return ()

        dataUpdate widget newV = do
            width <- getWidgetWidth widget
            v <- mkWidthsVector newV
            writeMonadIORef refD newV
            let maxIx = max 0 $ VU.length v - 1
            curIx <- toBound 0 maxIx <$> readMonadIORef refC
            off <- (toBound 0 maxIx . intrnOff) <$> readMonadIORef refS

            let newCurIx = case horizItsMoveOnUpdate of
                            MoveToFirstOnUpdate -> 0
                            MoveToLastOnUpdate -> maxIx
                            _ -> curIx
                newOff = if horizItsMoveOnUpdate == NoMoveOnUpdate then off
                         else let state = IntrnState off v
                                  (_,_,mainW) = scrollState width state
                              in calcOffForCurVisible mainW state newCurIx
            writeMonadIORef refS IntrnState { intrnOff = newOff, intrnWidths = v}
            writeMonadIORef refC newCurIx
            markWidgetForRedraw widget

        ixUpdate widget newV = do
            width <- getWidgetWidth widget
            state <- readMonadIORef refS
            let (_,_,mainW) = scrollState width state
            modifyMonadIORef' refS $ \s -> s{intrnOff = calcOffForCurVisible mainW state newV}
            writeMonadIORef refC newV
            readMonadIORef refH >>= (( $ newV) . hndlrOnMove)
            markWidgetForRedraw widget

    modifyMonadIORef' refH $ \h -> h{hndlrDataUpdate= dataUpdate, hndlrIxUpdate = ixUpdate}

    mkFormWidget horizItsFormItemDef horizItsFlags skin id
            (HorizItsData refD refH refC) parent fns{
      onDestroy = \ _widget -> do
            horizSepUnprepare gui horizSepPrData separatorsData
            viewableUnprepare gui dataPrep initData -- SDL.destroyTexture textureL >> SDL.destroyTexture textureR
      ,onMouseMotion = \widget btnsLst (P (V2 x _)) (V2 _relX _) -> do
            mold <- readMonadIORef refM
            m <- coord2ixM widget x
            m' <- if horizItsPermutable && (SDL.ButtonLeft `elem` btnsLst) then
                   case m of
                    ItemNo n -> do
                        ms <- readMonadIORef refMS
                        case ms of
                            ItemNo old -> do
                                m'' <- if abs (old - n) == 1 then do
                                            let ix = min old n
                                            modifyMonadIORef' refS $ \s ->
                                                s{intrnWidths = VU.swapNeighb ix $ intrnWidths s}
                                            f <- doNeighborSwap <$> readMonadIORef refH
                                            readMonadIORef refD >>= f ix >>= writeMonadIORef refD
                                            ixUpdate widget n
                                            coord2ixM widget x
                                       else return m
                                writeMS m''
                                return m''
                            _ -> return m
                    None -> return m
                    _ -> writeMonadIORef refMS None >> return m
                  else return m
            when (mold /=m') $ do
                writeMonadIORef refM m'
                markWidgetForRedraw widget
            setWidgetCursorIx widget (case m' of
                None -> SystemCursorArrow
                _ ->  SystemCursorHand )
      ,onMouseButton = \widget motion mouseButton clicks (P (V2 x _)) ->
            if motion==SDL.Pressed then do
                let doClick h ix | (clicks==1) && (mouseButton == SDL.ButtonLeft) = hndlrOnClk h ix
                                 | mouseButton == SDL.ButtonLeft = hndlrOnDblClk h ix
                                 | mouseButton == SDL.ButtonRight = hndlrOnRightClick h ix
                                 | otherwise = return ()
                state <- readMonadIORef refS
                r <- coord2ixM widget x
                writeMS r
                case r of
                    BtnLeft | (mouseButton == SDL.ButtonLeft) && (intrnOff state > 0) ->
                                writeMonadIORef refS state{intrnOff = intrnOff state -1} >>
                                                        markWidgetForRedraw widget
                    BtnRight | (mouseButton == SDL.ButtonLeft) &&
                                    (intrnOff state < (VU.length (intrnWidths state) - 1)) ->
                                writeMonadIORef refS state{intrnOff = intrnOff state +1} >>
                                                        markWidgetForRedraw widget
                    BtnOpt -> readMonadIORef refH >>= (`doClick` OptBtnIx) >> markWidgetForRedraw widget
                    ItemNo n -> do
                        writeMonadIORef refC n
                        h <- readMonadIORef refH
                        hndlrOnMove h n
                        doClick h n >> markWidgetForRedraw widget
                    _ -> return ()
            else writeMonadIORef refMS None >> writeMonadIORef refM None
      ,onLostMouseFocus = \widget -> writeMonadIORef refMS None >> writeMonadIORef refM None >> markWidgetForRedraw widget
      ,onDraw= \widget -> do
                rWidg@(SDL.Rectangle (P (V2 left _top)) (V2 width _height)) <- getVisibleRect widget
                draw3DFrame darkColor lightColor bkClr BorderThickness rWidg
                mouseSt <- readMonadIORef refM
                curIx <- readMonadIORef refC
                dat <- readMonadIORef refD
                state@IntrnState{..} <- readMonadIORef refS
                fl <- getWidgetFlags widget
                let maxIx = max 0 $ VU.length intrnWidths - 1
                    (leftBtEna,rightBtEna,mainW) = scrollState width state
                    SDL.Rectangle (P (V2 _internL internT)) (V2 _internW internH) =
                            shrinkRect' BorderThickness rWidg
                    rightX = left+PaddingX+mainW
                    btnsX  = rightX+PaddingX
                    drawSep x i  = do
                        let (w,sepPos) = if | i == 0 -> (horizSepPrFirstWidth,FirstSeparator)
                                            | i > maxIx -> (horizSepPrLastWidth,LastSeparator)
                                            | otherwise -> (horizSepPrMiddleWidth,MiddleSeparator)
                        horizSepDrawItem gui skin (SDL.Rectangle (P (V2 x internT)) (V2 w internH))
                                    horizSepPrData separatorsData sepPos
                                    (if | curIx == (i-1) -> LeftIsCurItem
                                        | curIx == i -> RightIsCurItem
                                        | otherwise -> OrdinalSeparator)
                        return $ x+w
                    widgetEna = (WidgetEnable .&. fl) /= WidgetNoFlags
                    widgetFocused = (WidgetFocused .&. fl) /= WidgetNoFlags
                    go x i | x >= rightX = return ()
                           | i > maxIx = void $ drawSep x i
                           | otherwise = do
                                x' <- drawSep x i
                                let w = intrnWidths VU.! i
                                viewableDrawItem gui skin (SDL.Rectangle (P (V2 x' internT)) (V2 w internH))
                                    (formDecore skin) widgetEna widgetFocused (mouseSt == ItemNo i)
                                    (i==curIx) dataPrep dat i
                                go (x'+w) $ i+1
                when (maxIx>=0) $
                    let fromX = left+PaddingX
                    in withClipRect (SDL.Rectangle (P (V2 fromX internT)) (V2 mainW internH)) $
                            go fromX intrnOff
                btnLRW <- if leftBtEna || rightBtEna then do
                            let drawArrBtn bt x orientation btEna = do
                                    let y = internT + ((internH - arrWidth) `div` 2)
                                        srcX = arrWidth * (if widgetEna && (mouseSt == bt) then 1 else 0)
                                        srcY = arrWidth * (if orientation == OrientationRight then 2 else 0)
                                        drawT = drawTexturePartial arrTexture (SDL.Rectangle (P (V2 srcX srcY))
                                                    (V2 arrWidth arrWidth)) (P (V2 x y))
                                    if btEna then drawT
                                    else withTransparentTexture 40 arrTexture drawT
                            drawArrBtn BtnLeft  btnsX            OrientationLeft  leftBtEna
                            drawArrBtn BtnRight (btnsX+arrWidth) OrientationRight rightBtEna
                            return $ 2*arrWidth
                          else return 0
                whenJust mbOptTexture $ \t ->
                    drawTexture t $ P $ V2 (btnsX+btnLRW) (internT + ((internH - optH) `div` 2))
                when ((WidgetFocused .&. fl) /= WidgetNoFlags) $ do
                    setColor $ formBorderColor skin
                    drawDotBorder 4 $ shrinkRect' 3 rWidg
                                     }

getWidgetWidth :: MonadIO m => Widget -> m Coord
getWidgetWidth = fmap (xV2 . sizeOfRect) . getWidgetRect

