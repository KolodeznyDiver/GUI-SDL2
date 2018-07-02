{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
-- |
-- Module:      GUI.Widget.Header
-- Copyright:   (c) 2017-2018 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Виджет размещаемый над настраиваемым виджетом, например, @listView@ для регулирования ширин колонок
-- и, возможно, вариантов сортировки.

module GUI.Widget.Header(
    -- * Типы используемые в header
    HeaderSortMode,HeaderDef(..),HeaderData
    -- * Установка функций-обработчиков событий
    ,onWidthsChange,onSortChange,
    -- * Функция создания виджета.
    header
    ) where

import Data.Maybe
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Bits
import Data.IORef
import qualified SDL
import SDL.Vect
import qualified SDL.Font as FNT
import Control.Monad.Extra (whenJust)
import Data.Default
import GUI
import GUI.Widget.Handlers

pattern PaddingX :: Coord
pattern PaddingX = 3
pattern PaddingY :: Coord
pattern PaddingY = 2
pattern SlideWidth :: Coord
pattern SlideWidth = 3 -- Ширина +- области в которой можно начать отображение колонки.
pattern ArrowTextureHW :: Coord -- Ширина и высота текстуры с треугольником
pattern ArrowTextureHW = 16
pattern ArrowHW :: Coord -- Размер треугольника
pattern ArrowHW = 10
pattern BorderThickness :: Coord
pattern BorderThickness = 1
pattern PressedOffset :: Coord -- На сколько смещать изображение на кнопке при нажатии на ней
pattern PressedOffset = 2      -- указателя мыши.

-- | Возможность и состояние сортировки по колонкам.
type HeaderSortMode = Maybe -- Если @Just@ - предолагается возможность сортировки нажатием на заголовки колонок.
                      (Int -- ^ Начальный номер колонки по которой предполагается сортировка.
                      ,SortMode -- ^ Режим сортировки.
                      )

-- | Начальные настройки виджета.
data HeaderDef = HeaderDef {
          headerFormItemDef :: FormItemWidgetDef -- ^ Общие настройки для всех виджетов для форм,
                                                   -- в настоящий момент только margin's.
                                                   -- Нижний margin обнуляется в @header@.
--        , headerSize       :: GuiSize -- ^ Размер без полей.
        , headerFlags      :: WidgetFlags -- ^ Флаги базового виджета.
        , headerColumns  :: V.Vector (T.Text,Coord) -- ^ Исходные заголовки и ширины колонок.
        , headerSortMode :: HeaderSortMode -- ^ Возможность и начальное состояние сортировки по колонкам.
                               }


instance Default HeaderDef where
    def = HeaderDef { headerFormItemDef = def
--                    , headerSize = zero
                    , headerFlags = WidgetVisible .|. WidgetEnable
                    , headerColumns = V.empty
                    , headerSortMode = Nothing
                    }

-- | Не экспортируемый тип записи. Хранится по ссылке в 'HeaderData'.
data HeaderHandlers = HeaderHandlers    { hdrOnWidthsChange :: forall m. MonadIO m => V.Vector Coord -> m ()
                                        , hdrOnSortChange :: forall m. MonadIO m => Int -> SortMode -> m ()
                                        }

-- | Тип созданного виджета. Обычно используется как  @GuiWidget ListViewData@.
data HeaderData = HeaderData { hdrWidths :: VUM.IOVector Coord
                             , hdrSortMode :: Maybe (IORef (Int,SortMode))
                             , hdrHndlrs :: IORef HeaderHandlers
                             }

-- | Установка функции-обработчика на изменение ширин колонок.
onWidthsChange :: MonadIO m => GuiWidget HeaderData -> (forall n. MonadIO n => V.Vector Coord -> n ()) -> m ()
onWidthsChange w a = modifyMonadIORef' (hdrHndlrs $ widgetData w) (\d -> d{hdrOnWidthsChange= a})

-- | Установка функции-обработчика на изменение режима сортировки.
onSortChange :: MonadIO m => GuiWidget HeaderData -> (forall n. MonadIO n => Int -> SortMode -> n ()) -> m ()
onSortChange w a = modifyMonadIORef' (hdrHndlrs $ widgetData w) (\d -> d{hdrOnSortChange= a})

-- | Извлечение состояния виджета для сохранения.
instance GetStateForSave (GuiWidget HeaderData) (V.Vector Coord,HeaderSortMode) where
    getStateForSave w = do
        let HeaderData{..} = widgetData w
        widths <- liftIO $ V.generateM (VUM.length hdrWidths) (VUM.read hdrWidths)
        srtMd <- fmapMaybeM readMonadIORef hdrSortMode
        return (widths,srtMd)

-- | Не экспортируемый тип. Определяет в какой области виджета находится указатель.
data AreaOfMouse = Btn  Int -- ^ На кнопке.
                 | Sldr Int -- ^ Между кнопками, там где имитация слайдера.

-- | Функция создания виджета.
header :: MonadIO m => HeaderDef -> -- ^ Параметры виджета.
                         Widget -> -- ^ Будующий предок в дереве виджетов.
                         Skin -> -- ^ Skin.
                         m (GuiWidget HeaderData)
header HeaderDef{..} parent skin = do
    let bkClr = decoreBkColor $ formDecore skin
        fgClr = decoreFgColor $ formDecore skin
    (fnt,textureUp,textureDn) <- runProxyCanvas parent $ do
        f <- getFont "small"
        let sz = V2 ArrowTextureHW ArrowTextureHW
            r = SDL.Rectangle zero sz
            mkArrTexture orientation = do
                t <- createTargetTexture sz
                withTargetTexture t $ do
                    setColor bkClr
                    fillRect r
                    drawArrowTriangle orientation fgClr (rectCenter r) ArrowHW
                return t
        tUp <- mkArrTexture OrientationUp
        tDn <- mkArrTexture OrientationDown
        return (f,tUp,tDn)
    fntHeight <- FNT.lineSkip fnt -- FNT.height fnt
    let cCols = V.length headerColumns
        widgW = V.foldr (\(_,w) -> (w+)) 0 headerColumns
        widgH = fntHeight + 2*PaddingY
        widgSz = V2 widgW widgH
        lightColor = brdr3DLightColor $ brdr3DColors skin
        darkColor  = brdr3DDarkColor  $ brdr3DColors skin
    widths <- liftIO $ do
        v <- VUM.new cCols
        V.imapM_ (\i (_,w) -> VUM.write v i w) headerColumns
        return v
    sortMode <- fmapMaybeM newMonadIORef headerSortMode
    rfH <- newMonadIORef $ HeaderHandlers (\_ -> return ()) (\_ _-> return ())
    rfBtnPressed <- newMonadIORef (-1)
    let doWidthsChange widget ix delta = when (ix < (cCols-1)) $ do
            chgd <- liftIO $ do
                wl <- VUM.read widths ix
                wr <- VUM.read widths $ ix+1
                if | delta < 0 -> let w' = max (SlideWidth*2) $ wl + delta
                                      d  = wl - w'
                                  in if d == 0 then return False
                                     else VUM.write widths ix w' >> VUM.write widths (ix+1) (wr+d) >> return True
                   | delta > 0 -> let w' = max (SlideWidth*2) $ wr - delta
                                      d  = wr - w'
                                  in if d == 0 then return False
                                     else VUM.write widths ix (wl+d) >> VUM.write widths (ix+1) w' >> return True
                   | otherwise -> return False
            when chgd $ do
                markWidgetForRedraw widget
                v <- liftIO $ V.generateM cCols (VUM.read widths)
                f <- hdrOnWidthsChange <$> readMonadIORef rfH
                f v
        doSortChange widget rf ixOld smOld ix sm =
            when (ix /= ixOld || sm /= smOld) $ do
                writeMonadIORef rf (ix,sm)
                markWidgetForRedraw widget
                f <- hdrOnSortChange <$> readMonadIORef rfH
                f ix sm
        coord2ix xPos = liftIO $ do
            let go x i | i >= (cCols-1) = return $ Btn i
                       | otherwise = do
                            x' <- (x+) <$> VUM.read widths i
                            if | x' < xPos -> go x' (i+1)
                               | (x'-SlideWidth*2) < xPos -> return $ Sldr i
                               | otherwise -> return $ Btn i
            if SlideWidth < xPos then go SlideWidth 0 else return $ Btn 0

        coord2ix' widget xPos = do
            r <- coord2ix xPos
            setWidgetCursorIx widget (case r of
                Sldr _ ->  SystemCursorSizeWE
                _ -> SystemCursorArrow)
            return r
        mouseDepressed widget = do
            i <- readMonadIORef rfBtnPressed
            when (i>=0)
                (writeMonadIORef rfBtnPressed (-1) >> markWidgetForRedraw widget)
    mkFormWidget headerFormItemDef headerFlags skin
            (\(MarginLTRB l t r _) -> MarginLTRB l t r 0)
            (HeaderData widths sortMode rfH) parent (noChildrenFns widgSz){
      onDestroy = \ _widget -> SDL.destroyTexture textureUp >> SDL.destroyTexture textureDn
      ,onMouseMotion = \widget btnsLst (P (V2 x _)) (V2 relX _) ->
            if SDL.ButtonLeft `elem` btnsLst then do
                k <- coord2ix' widget $ x - relX
                case k of
                   Sldr i -> doWidthsChange widget i relX
                   _ -> return ()
            else void $ coord2ix' widget x
      ,onMouseButton = \widget motion mouseButton _clicks (P (V2 x _)) ->
            if (motion==SDL.Pressed) && (mouseButton == SDL.ButtonLeft) then
                whenJust sortMode $ \rf -> do
                    k <- coord2ix x
                    case k of
                       Btn i -> do
                            (ixOld,smOld) <- readMonadIORef rf
                            doSortChange widget rf ixOld smOld i
                                (if ixOld /= i then Ascending
                                 else invSortMode smOld)
                            writeMonadIORef rfBtnPressed i
                       _ -> mouseDepressed widget
            else mouseDepressed widget
      ,onLostMouseFocus = \widget -> mouseDepressed widget
      ,onDraw= \_widget -> do
--                fl <- getWidgetFlags widget
                iBtnPressed <- readMonadIORef rfBtnPressed
                srtMd <- fmapMaybeM readMonadIORef sortMode
                let iSortCol = maybe (-1) fst srtMd
                    go x i | i >= cCols = return ()
                           | otherwise = do
                                w <- liftIO $ VUM.read widths i
                                let isPressed = iBtnPressed == i
                                    isSortCol = iSortCol == i
                                    (ltColor,rbColor,off) = if isPressed then (darkColor,lightColor,PressedOffset)
                                                            else (lightColor,darkColor,0)
                                    txtX = x+PaddingX+off
--                                    txtP = P (V2 txtX (PaddingY+off-2))
                                    txtW = w - 2*PaddingX - (if isSortCol then ArrowTextureHW else 0)
                                    internH = widgH-2*BorderThickness
                                draw3DFrame ltColor rbColor bkClr BorderThickness
                                    $ SDL.Rectangle (P (V2 x 0)) (V2 w widgH)
                                withClipRect (SDL.Rectangle (P (V2 txtX (PaddingY*2))) (V2 txtW internH)) $
                                    drawTextOpaque fnt fgClr bkClr (P (V2 txtX (PaddingY+off-2))) $
                                        fst $ headerColumns V.! i
                                when isSortCol $
                                    drawTexture (if snd (fromJust srtMd) == Ascending then textureUp
                                                 else textureDn) $
                                            P (V2 (x+w-PaddingX-ArrowTextureHW+off)
                                                  (((widgH-ArrowTextureHW) `div` 2)+off))
                                go (x+w) $ i+1
                go 0 0
                                     }

invSortMode :: SortMode -> SortMode
invSortMode Ascending = Descending
invSortMode Descending = Ascending
{-# INLINE invSortMode #-}

-- copypast from ghc package
-- | Monadic version of fmap
fmapMaybeM :: (Monad m) => (a -> m b) -> Maybe a -> m (Maybe b)
fmapMaybeM _ Nothing  = return Nothing
fmapMaybeM f (Just x) = fmap Just (f x) -- f x >>= (return . Just)
