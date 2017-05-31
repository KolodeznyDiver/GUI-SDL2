{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GUI.Window.PopupMenu(
    MenuItem(..), MenuItems, DynMenuItem(..)
    ,mkMenu,mItem,mItemSub,popupMenu
    ) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Data.Ix
import Data.Bits
import System.FilePath
import Maybes (whenIsJust)
import qualified SDL
import SDL.Vect
import Data.StateVar
import GUI
import qualified GUI.BaseLayer.Primitives as P
import GUI.BaseLayer.PopupWindow

class MenuMaker r where
    menuMaker :: MenuItems -> r

instance MenuMaker MenuItems where
    menuMaker = id

instance MenuMaker r => MenuMaker (MenuItem -> r) where
    menuMaker menu = \i -> menuMaker $ V.snoc menu i

mkMenu :: MenuMaker r => r
mkMenu = menuMaker V.empty

mItem :: T.Text -> T.Text -> MenuItem
mItem g k = MenuItem g k V.empty

mItemSub :: T.Text -> T.Text -> MenuItems -> MenuItem
mItemSub g k s = MenuItem g k s

popupMenu :: MonadIO m => Widget -> GuiRect -> MenuItems -> m ()
popupMenu parent rect vm = do
    parentWin <- getWidgetWindow parent
    isParentPopup <- allWindowFlags parentWin WindowPopupFlag
    mbHMenu <- getWinMainMenu parentWin
    let isParentHMenu = mbHMenu == Just parent
--    void $ mkPopupWindow parent rect
    win <- mkPopupWindow parent rect
    when isParentPopup $
        windowFlagsRemove parentWin WindowCloseOnLostFocuse
    void $ win $+ popupMenuWidget PopupMenuWidgetDef{
         popupMenuWidgetItems=vm
        ,popupMenuHMenu = if isParentHMenu then Just parent else Nothing
        ,popupPrevWin = if isParentPopup  then Just parentWin else Nothing
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

data MenuItem = MenuItem { menuItemGroup :: T.Text
                         , menuItemName  :: T.Text
                         , menuItemSubmenu :: V.Vector MenuItem
                         }
              | MenuItemsGenerator (forall m. MonadIO m => m (V.Vector DynMenuItem))

type MenuItems = V.Vector MenuItem

data DynMenuItem = DynMenuItem { dynMenuCaption :: T.Text
                               , dynMenuAction :: forall m. MonadIO m => m ()
                               }

-- no exported. Item of current active menu
data Item = Item   { itemText :: T.Text
                   , itemPicture  :: Maybe SDL.Texture
                   , itemEnable :: Bool
                   , itemType :: ItemType
                   }
           | SeparatorItem
-- no exported.
data ItemType = ActionItem { itemHkTxt :: T.Text
                           , actionItem :: forall m. MonadIO m => m ()
                           }
              | SubmenuItem MenuItems
-- no exported.
data ItemCoord = ItemCoord { itemY :: Coord, itemH :: Coord}

data PopupMenuWidgetDef = PopupMenuWidgetDef { popupMenuWidgetItems :: MenuItems
                                             , popupMenuHMenu :: Maybe Widget
                                             , popupPrevWin :: Maybe GuiWindow
                                             }

popupMenuWidget :: MonadIO m => PopupMenuWidgetDef -> Widget -> Skin -> m (GuiWidget SimpleWidget)
popupMenuWidget PopupMenuWidgetDef{..} parent skin = do
    win <- getWidgetWindow parent
    gui <- getGuiFromWindow win
    guiStOnInit <- getGuiState gui
    selectedItNum <- newMonadIORef (-1)
    fnt <- runProxyCanvas parent $ getFont "menu"
    let addSeparator = (`V.snoc` SeparatorItem)
        dynItemMaker (w,h,v) DynMenuItem{..} = do
            (V2 tW tH) <- P.strSize fnt $ T.unpack dynMenuCaption
            return (max w tW, max h tH, v `V.snoc` Item dynMenuCaption Nothing True
                                                        (ActionItem T.empty dynMenuAction))
        itemMaker r@(pictPresent,mainW,maxH,v,curGroup) MenuItem{..} = do
            mbA <- getActionByGroupAndId gui menuItemGroup menuItemName
            case mbA of
                Just Action{..} -> do
                    let isSubmenu = not $ V.null menuItemSubmenu
                        isPict = not $ T.null actionPicture
                        hkTxt = maybe T.empty (T.pack . show) actionHotKey
                        v' = if not (V.null v) && curGroup /= Just menuItemGroup then addSeparator v else v
                    mbT <- if isPict then
                                Just <$> runProxyCanvas parent
                                        (getTexture $ getMenuPictWithDirectory actionPicture)
                           else return Nothing
                    (V2 tW tH) <- P.strSize fnt $ T.unpack $ T.append actionText hkTxt
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
        returnToPrevPopup = do
            close
            whenIsJust popupPrevWin $ \ prevWin -> do
                SDL.raiseWindow =<< getSDLWindow prevWin
                windowFlagsAdd prevWin WindowCloseOnLostFocuse
        close :: MonadIO m => m ()
        close =
            delWindow win
--        doSubmenu :: MonadIO m => Widget -> MenuItems -> m ()
        doSubmenu widget i subMenu = -- do
--            liftIO $ putStrLn $ concat ["PopupMenu.doSubmenu : first menuItemName = "
--                , T.unpack $ menuItemName $ V.head subMenu]
            popupMenu widget (SDL.Rectangle (P(V2 winW (itemY $ itemsCoord V.! i))) (V2 10 10)) subMenu

--        doItem :: MonadIO m => Widget -> Int -> m ()
        doItem widget i = when (i>=0) $ case items V.! i of
            Item{itemType=ActionItem{actionItem=f}} -> close >> f
            Item{itemType=(SubmenuItem subMenu)} -> doSubmenu widget i subMenu
            _ -> return ()
        fns = noChildrenFns winSz
    setWinSize win winSz
    mkWidget (WidgetVisible .|. WidgetEnable .|. WidgetFocusable) WidgetMarginNone
            SimpleWidget parent fns{
        onCreate = \widget -> onCreate fns widget >> setWidgetFocus widget
        ,onDestroy = \ _widget -> whenIsJust popupPrevWin $ \ prevWin -> do
            winSDL <- getSDLWindow prevWin
            SDL.showWindow winSDL >> SDL.raiseWindow winSDL >> windowFlagsAdd prevWin WindowCloseOnLostFocuse
        ,onLostMouseFocus = \widget -> setSelected widget (-1)
--        ,onLostKeyboardFocus = \widget -> setActive widget (-1)
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
                                SDL.KeycodeEscape -> returnToPrevPopup
                                _ -> return ())
--        ,onNotify = \ _widget _notifyCode _mbSrcWidget -> return ()
        ,onDraw= \widget -> do
                nSel <- readMonadIORef selectedItNum
                r@(SDL.Rectangle _ (V2 fullW _)) <- getVisibleRect widget
                draw3DFrame (popupMnu3DLightColor skin) (popupMnu3DDarkColor skin)
                            (popupMnuBkColor skin) BorderThickness r

                (`V.imapM_` items) $ \ i t -> do
                    let ItemCoord{..}= itemsCoord V.! i
                        bkClr = (if i==nSel then popupMnuInColor else popupMnuBkColor) skin
                        yCenter = itemY + itemH `div` 2
                    when (i==nSel) $ do
                        setColor $ popupMnuInColor skin
                        fillRect (SDL.Rectangle (P(V2 BorderThickness itemY)) (V2 (fullW - 2*BorderThickness) itemH))
                    case t of
                        Item{..} -> do
                            let (tColor,hkColor) | itemEnable = (popupMnuFgColor skin,popupMnuHotKeyColor skin)
                                                 | otherwise = (disableFgColor skin,disableFgColor skin)
                            whenIsJust itemPicture $ \ texture -> do
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
                        _ -> setColor (popupMnuSeparatorColor skin) >>
                                drawLine (P (V2 (BorderThickness+LeftPadding) yCenter))
                                         (P (V2 (fullW - BorderThickness - RightPadding) yCenter))

            }

getMenuPictWithDirectory :: T.Text -> T.Text
getMenuPictWithDirectory = T.pack . ("menu" </>) . T.unpack
