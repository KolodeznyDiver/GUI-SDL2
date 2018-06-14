{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module:      GUI.Window.PopupMenu
-- Copyright:   (c) 2017-2018 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Окно вспывающего вертикального меню. Может быть каскадным и сочететься с горизонтальным меню,
-- см. "GUI.Widget.Menu.Horizontal"
--
-- Картинки (иконки) для пунктов меню указанные в соотвествующих
-- 'GUI.BaseLayer.Depend1.Action.Action' безутся из подкаталога \"menu\" каталога ресурсов.

module GUI.Window.PopupMenu(
    -- * Типы которыми описывается вертикальное каскадное меню.
    MenuItem(..), DynMenuItem(..), MenuItems
    -- * Вспомогательные функции для упрощения описания меню.
    ,mkMenu,mItem,mItemSub
    -- * Функция создающая выпадающее меню.
    ,popupMenu
    ) where

import Data.Monoid
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified TextShow as TS
import           Data.ByteString.Char8   (ByteString)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Data.Ix
import Data.Bits
import System.FilePath
import Control.Monad.Extra (whenJust)
import qualified SDL
import SDL.Vect
import SDL.Internal.Numbered (FromNumber(..))
import GUI
import qualified GUI.BaseLayer.Primitives as P
import GUI.BaseLayer.PopupWindow
import GUI.Widget.Handlers

-- | Класс поддержки polyvariadic функции @mkMenu@ для построения вектора элементов меню.
class MenuMaker r where
    menuMaker :: MenuItems -> r

instance MenuMaker MenuItems where
    menuMaker = id

instance MenuMaker r => MenuMaker (MenuItem -> r) where
    menuMaker menu = menuMaker . V.snoc menu

-- | Polyvariadic функция для построения вектора элементов вертикального меню.
-- Хотя, вместо
--
-- > mkMenu (mItem "Edit" "Cut") (mItem "Edit" "Copy") (mItem "Edit" "Paste")
--
-- Можно и
--
-- > V.fromList [mItem "Edit" "Cut", mItem "Edit" "Copy", mItem "Edit" "Paste"]
--
mkMenu :: MenuMaker r => r
mkMenu = menuMaker V.empty

-- | Функция создания описания одного пункта меню без подменю.
mItem :: ByteString -> ByteString -> MenuItem
mItem g k = MenuItem g k V.empty

-- | Функция создания описания одного пункта меню с подменю.
mItemSub :: ByteString -> ByteString -> MenuItems -> MenuItem
mItemSub = MenuItem

-- | Функция создающая окно с выпадающим меню.
popupMenu :: MonadIO m =>
                -- | Виджет активного сейчас окна. Popup окно станет дочерним по отношению к этому окну.
                Widget ->
                -- | Начальные координаты окна в координатах указанного виджета.
                GuiRect ->
                MenuItems ->  -- ^ Вектор пунктов меню.
                m ()
popupMenu parent rect vm = do
    parentWin <- getWidgetWindow parent
    isParentPopup <- allWindowFlags parentWin WindowPopupFlag
    mbHMenu <- getWinMainMenu parentWin
    let isParentHMenu = mbHMenu == Just parent
    win <- mkPopupWindow parent rect
    when isParentPopup $
        windowFlagsRemove parentWin WindowCloseOnLostFocuse
    void $ win $+ popupMenuWidget PopupMenuWidgetDef{
         popupMenuWidgetItems=vm
        ,popupPrev = if | isParentPopup -> PrevPopup parentWin
                        | isParentHMenu -> PrevHMenu parent
                        | otherwise -> PrevNothing
                                                    }

------------------------- Widget for Popup menu window -------------------------------

pattern LeftPadding :: Coord
pattern LeftPadding = 13
pattern RightPadding :: Coord
pattern RightPadding = 13
pattern InterTextPaddingX :: Coord
pattern InterTextPaddingX = 36
pattern PictPaddingX :: Coord
pattern PictPaddingX = 5
pattern PictPaddingY :: Coord
pattern PictPaddingY = 2
pattern PictW :: Coord
pattern PictW = 16
pattern PictH :: Coord
pattern PictH = 16
pattern PaddingY :: Coord
pattern PaddingY = 3
pattern SeparatorH :: Coord
pattern SeparatorH = 6
pattern ArrowW :: Coord
pattern ArrowW = 8
pattern BorderThickness :: Coord
pattern BorderThickness = 1

-- | Определение или одного пункта меню, либо генератора нескольких пунктов меню.
-- Пункты меню взятые из разных груп 'GUI.BaseLayer.Depend1.Action.Action' разделяются
-- вертикальной чертой. Так же разделяются от остальных пунктов меню, пункты созданные с помощью
-- генераторов пунктов меню
data MenuItem =
        -- | Обычный пункт меню. Основная информацию о нём определяется
        -- указанным в нём 'GUI.BaseLayer.Depend1.Action.Action'
        MenuItem {
              menuItemGroup :: ByteString -- ^ Имя группы 'Action'.
            , menuItemName  :: ByteString -- ^ Идентификатор в группе 'Action'.
                                          -- Эти два элемента позволяют ссылаться на определённый 'Action'.
            , menuItemSubmenu :: V.Vector MenuItem -- ^ Вектор элементов подменю выпадающего каскадно.
                                                   -- Пустой вектор, если подменю не требуется.
                 }
      -- | Вместо пункта меню может задан генератор - функция генерирующая пункты меню,
      -- например список недавно открываемых файлов или список плагинов.
      | MenuItemsGenerator (forall m. MonadIO m => m (V.Vector DynMenuItem))

-- | Вектор пунктов меню.
type MenuItems = V.Vector MenuItem

-- | Элементы вектора генерируемые генератором пунктов меню.
-- Генератор создаёт пункты не привязанные к 'Action', Отображаемые только текстовыми строками,
-- без подпунктов меню.
data DynMenuItem = DynMenuItem {
          dynMenuCaption :: T.Text -- ^ Заголовок пункта меню.
        , dynMenuAction :: forall m. MonadIO m => m ()  -- ^ Функция, вызываемая при выборе пункта меню.
                               }

-- | no exported. Item of current active menu
data Item = Item   { itemText :: T.Text
                   , itemPicture  :: Maybe SDL.Texture
                   , itemEnable :: Bool
                   , itemType :: ItemType
                   }
           | ItemSeparator

-- no exported.
data ItemType = ActionItem { itemHkTxt :: T.Text
                           , actionItem :: forall m. MonadIO m => m ()
                           }
              | SubmenuItem MenuItems

-- no exported.
data ItemCoord = ItemCoord { itemY :: Coord, itemH :: Coord}

-- | no exported. Ссылка на предыдущее меню.
data PopupMenuWidgetPrev =
        PrevHMenu Widget -- ^ Предыдущее меню - горизонтальное меню.
      | PrevPopup Window -- ^ Предыдущее меню - такое же вертикальное меню.
      | PrevNothing -- ^ Предыдущего меню не было.

-- | Описание параметров виджета заполняющего окно вертикального меню. Не экспортируется.
data PopupMenuWidgetDef = PopupMenuWidgetDef {
          popupMenuWidgetItems :: MenuItems -- ^ Вектор пунктов меню.
        , popupPrev :: PopupMenuWidgetPrev  -- ^ Ссылка на предыдущее меню.
                                             }

-- | Функция создания виджета в вертикальном меню. Не экспортируется.
popupMenuWidget :: MonadIO m => PopupMenuWidgetDef -> Widget -> Skin -> m (GuiWidget SimpleWidget)
popupMenuWidget PopupMenuWidgetDef{..} parent skin = do
    win <- getWidgetWindow parent
    gui <- getWindowGui win
    guiStOnInit <- getGuiState gui
    selectedItNum <- newMonadIORef (-1)
    needPrevRestore <- newMonadIORef True
    fnt <- runProxyCanvas parent $ getFont "menu"
    let addSeparator = (`V.snoc` ItemSeparator)
        dynItemMaker (w,h,v) DynMenuItem{..} = do
            (V2 tW tH) <- P.textSize fnt dynMenuCaption
            return (max w tW, max h tH, v `V.snoc` Item dynMenuCaption Nothing True
                                                        (ActionItem T.empty dynMenuAction))
        itemMaker r@(pictPresent,mainW,maxH,v,curGroup) MenuItem{..} = do
            mbA <- getActionByGroupAndId gui menuItemGroup menuItemName
            case mbA of
                Just Action{..} -> do
                    let isSubmenu = not $ V.null menuItemSubmenu
                        isPict = not $ T.null actionPicture
                        hkTxt = maybe T.empty (TS.toText . TS.showb) actionHotKey
                        v' = if not (V.null v) && curGroup /= Just menuItemGroup then addSeparator v else v
                    mbT <- if isPict then
                                Just <$> runProxyCanvas parent
                                        (getTexture $ getMenuPictWithDirectory actionPicture)
                           else return Nothing
                    (V2 tW tH) <- P.textSize fnt $ T.append actionText hkTxt
                    let w | isSubmenu = tW + InterTextPaddingX + ArrowW
                          | T.null hkTxt = tW
                          | otherwise = tW + InterTextPaddingX
                        h = max (if isPict then PictPaddingY*2+PictH else 0) (PaddingY + tH)
                    return (pictPresent || isPict, max mainW w, max maxH h
                            , v' `V.snoc` Item actionText mbT
                                                (isVisibleActionState $ getActionValueState guiStOnInit actionValue)
                                                (if isSubmenu then SubmenuItem menuItemSubmenu
                                                 else ActionItem hkTxt $ onAction actionValue)
                            , Just menuItemGroup)
                _ -> return r
        itemMaker r@(pictPresent,mainW,maxH,v,_curGroup) (MenuItemsGenerator gFn) = do
            d <- gFn
            if V.null d then return r
            else do let v' = addSeparator v
                    (w,h,v'') <- V.foldM' dynItemMaker (0,0,V.empty) d
                    return (pictPresent,max mainW w, max maxH (PaddingY + h),v' V.++ v'',Nothing)
    (pictPresent,mainW,maxH,items,_)
        <- V.foldM' itemMaker (False,0,0,V.empty,Nothing) popupMenuWidgetItems
    let cItems = V.length items
        itemsCoord :: V.Vector ItemCoord
        itemsCoord = V.create $ do
            v <- VM.new cItems
            V.ifoldM'_ (\y i t -> let h = case t of
                                            Item{} -> maxH
                                            _ -> SeparatorH
                                  in VM.write v i (ItemCoord y h) >> return (y + h)
                       ) BorderThickness items
            return v
        xText = BorderThickness + (if pictPresent then 2*PictPaddingX + PictW else LeftPadding)
        winW = xText + mainW + RightPadding + BorderThickness
        winSz = V2 winW (let ItemCoord{..}= V.last itemsCoord in itemY + itemH + BorderThickness )
        isEnable i | i>=0, Item{itemEnable=b} <- items V.! i = b
                   | otherwise = False
        getItemNumFromY x =
            case V.findIndex (\ItemCoord{..} -> inRange (itemY,itemY+itemH) x) itemsCoord of
                        Just i | isEnable i -> i
                        _ -> -1
        setSelected widget i = writeMonadIORef selectedItNum i >> markWidgetForRedraw widget
        setNextItem widget i = do
              let go n | n == i = return ()
                       | n == cItems = go 0
                       | isEnable n = setSelected widget n
                       | otherwise = go $ n+1
              go $ i+1
        setPrevItem widget i = do
              let go n | n == i = return ()
                       | n <0 = go $ cItems - 1
                       | isEnable n = setSelected widget n
                       | otherwise = go $ n-1
              go $ i-1
        returnToPrevPopup :: MonadIO m => m ()
        returnToPrevPopup = -- do
            close
{-            case popupPrev of
             PrevPopup prevWin -> do
                SDL.raiseWindow =<< getSDLWindow prevWin
                windowFlagsAdd prevWin WindowCloseOnLostFocuse
             _ -> return () -}
        close :: MonadIO m => m ()
        close =
            delWindow win
--        doSubmenu :: MonadIO m => Widget -> MenuItems -> m ()
        doSubmenu widget i subMenu = -- do
--            liftIO $ putStrLn $ concat ["PopupMenu.doSubmenu : first menuItemName = "
--                , T.unpack $ menuItemName $ V.head subMenu]
            popupMenu widget (SDL.Rectangle (P(V2 winW (itemY $ itemsCoord V.! i))) (V2 10 10)) subMenu

        restoreHMenu hmWidget = do
            getWidgetWindow hmWidget >>= (`windowFlagsRemove` WindowClickable)
            hmFns <- getWidgetFns hmWidget
            -- \widget motion _repeated keycode km
            logOnErr gui "popupMenuWidget.restoreHMenu.onKeyboard" $
                onKeyboard hmFns hmWidget SDL.Pressed False SDL.KeycodeEscape $ fromNumber 0
--        doItem :: MonadIO m => Widget -> Int -> m ()
        doItem widget i = when (i>=0) $ case items V.! i of
            Item{itemType=ActionItem{actionItem=f},itemText=txt} -> do
                writeMonadIORef needPrevRestore False
                case popupPrev of
                    PrevHMenu hmWidget -> restoreHMenu hmWidget
                    _ -> return ()
                delAllPopupWindows gui
                logOnErr gui ("popupMenu item \"" <> TS.fromText txt <> "\"") f
            Item{itemType=(SubmenuItem subMenu)} -> doSubmenu widget i subMenu
            _ -> return ()
        fns = noChildrenFns winSz
    setWinSize win winSz
    mkWidget (WidgetVisible .|. WidgetEnable .|. WidgetFocusable) WidgetMarginNone
            SimpleWidget parent fns{
        onCreate = \widget -> onCreate fns widget >> setWidgetFocus widget
        ,onDestroy = \ _widget -> do
            b <- readMonadIORef needPrevRestore
            when b $ case popupPrev of
                PrevHMenu hmWidget -> restoreHMenu hmWidget
                PrevPopup prevWin -> do
                    winSDL <- getSDLWindow prevWin
                    SDL.showWindow winSDL >> SDL.raiseWindow winSDL
                    windowFlagsAdd prevWin WindowCloseOnLostFocuse
                _ -> return ()
        ,onLostMouseFocus = \widget -> setSelected widget (-1)
        ,onMouseMotion = \widget _btnsLst (P (V2 _ y)) _relMv -> do
            let n = getItemNumFromY y
            o <- readMonadIORef selectedItNum
            when (n /= o) (setSelected widget n)
        ,onMouseButton = \widget motion mouseButton _clicks (P (V2 _ y)) ->
          when ((motion==SDL.Pressed) && (mouseButton == SDL.ButtonLeft))
            (doItem widget $ getItemNumFromY y)
        ,onKeyboard = \widget motion _repeated keycode km -> do
--            liftIO $ putStrLn $ concat ["horizontalMenu.onKeyboard "]
            let shiftCtrlAlt = getShftCtrlAlt km
            when (motion==SDL.Pressed && shiftCtrlAlt == ShiftCtrlAlt False False False) $ do
                o <- readMonadIORef selectedItNum
                when (o >= 0)
                    (if isEnterKey keycode || keycode== SDL.KeycodeSpace then doItem widget o
                     else case keycode of
                                SDL.KeycodeUp -> setPrevItem widget o
                                SDL.KeycodeDown -> setNextItem widget o
                                SDL.KeycodeRight | Item{itemType=(SubmenuItem subMenu)}
                                                        <- items V.! o -> doSubmenu widget o subMenu
                                SDL.KeycodeLeft   -> returnToPrevPopup
--                                SDL.KeycodeEscape -> returnToPrevPopup
                                _ -> return ())
        ,onDraw= \widget -> do
                nSel <- readMonadIORef selectedItNum
                r@(SDL.Rectangle _ (V2 fullW _)) <- getVisibleRect widget
                draw3DFrame (brdr3DLightColor $ popupMnuBorderColors skin)
                            (brdr3DDarkColor  $ popupMnuBorderColors skin)
                            (decoreBkColor (popupMnuDecore skin)) BorderThickness r
                (`V.imapM_` items) $ \ i t -> do
                    let ItemCoord{..}= itemsCoord V.! i
                        bkClr = (if i==nSel then popupMnuInColor else decoreBkColor . popupMnuDecore) skin
                        yCenter = itemY + itemH `div` 2
                    when (i==nSel) $ do
                        setColor $ popupMnuInColor skin
                        fillRect (SDL.Rectangle (P(V2 BorderThickness itemY)) (V2 (fullW - 2*BorderThickness) itemH))
                    case t of
                        Item{..} -> do
                            let (tColor,hkColor) | itemEnable = (decoreFgColor (popupMnuDecore skin),popupMnuHotKeyColor skin)
                                                 | otherwise = (popupMnuDisabledFgColor skin,popupMnuDisabledFgColor skin)
                            whenJust itemPicture $ \ texture -> do
                                let texturePos = P (V2 (BorderThickness + PictPaddingX) (itemY + PictPaddingY))
                                if itemEnable then drawTexture texture texturePos
                                else withTransparentTexture 40 texture $ drawTexture texture texturePos
                            drawTextOpaque fnt tColor bkClr (P (V2 xText itemY)) itemText
                            case itemType of
                                ActionItem{..} ->
                                     drawTextAligned fnt AlignRightCenter hkColor (DrawStrOpaque bkClr)
                                                    (SDL.Rectangle (P (V2 xText itemY)) (V2 mainW maxH)) itemHkTxt
                                _ -> setColor hkColor >> drawArrow OrientationRight
                                        (P (V2 (fullW - (ArrowW `div` 2) - BorderThickness - RightPadding) yCenter))
                                        (ArrowW `div` 2)
                        -- ItemSeparator
                        _ -> setColor (popupMnuSeparatorColor skin) >>
                                drawLine (P (V2 (BorderThickness+LeftPadding) yCenter))
                                         (P (V2 (fullW - BorderThickness - RightPadding) yCenter))
            }

getMenuPictWithDirectory :: T.Text -> T.Text
getMenuPictWithDirectory = T.pack . ("menu" </>) . T.unpack
