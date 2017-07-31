{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:      GUI.Widget.Menu.Horizontal
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <kolodeznydiver@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Виджет горизонтального меню из которого могут вызываться @GUI.Window.PopupMenu@

module GUI.Widget.Menu.Horizontal(
    -- GUI.Widget.Menu.Internal.Popup
    MenuItem(..), MenuItems, DynMenuItem(..)
    -- GUI.Window.PopupMenu
    ,mkMenu,mItem,mItemSub
    -- GUI.Widget.Menu.Horizontal
    ,HMenuItem(..), HorizontalMenuDef(..),HorizontalMenuData
    ,mkHMenu,horizontalMenu
    ) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Data.Ix
import Data.Bits
import qualified SDL
import SDL.Vect
import Data.Default
import GUI
import qualified GUI.BaseLayer.Primitives as P
import GUI.Window.PopupMenu
import GUI.Widget.Handlers

pattern PaddingX :: Coord
pattern PaddingX = 7
pattern PaddingY :: Coord
pattern PaddingY = 3

-- | Описание элемента (пункта) горизонтального меню
data HMenuItem = HMenuItem {
          hmenuText :: T.Text -- ^ Текст в меню.
        , hmenuMask :: ActionMask -- ^ Маска задающая состояние пункта меню в зависимости от
                                  -- кода состояния приложения. См. "GUI.BaseLayer.Depend1.Action".
        , hmenuPopup :: MenuItems -- Вектор элементов вертикального (выпадающего) меню с которым
                                  -- выпадающее меню будет вызвано при выборе этого пункта меню.
                            }

instance Default HMenuItem where
    def = HMenuItem  { hmenuText = T.empty
                     , hmenuMask = AllEnableActionMask
                     , hmenuPopup = V.empty
                     }

-- | Класс поддержки polyvariadic функции @mkHMenu@ для построения вектора элементов меню.
class HMenuMaker r where
    hmenuMaker :: V.Vector HMenuItem -> r

instance HMenuMaker (V.Vector HMenuItem) where
    hmenuMaker = id

instance HMenuMaker r => HMenuMaker (HMenuItem -> r) where
   hmenuMaker menu = hmenuMaker . V.snoc menu

-- | Polyvariadic функция для построения вектора элементов меню.
-- Хотя, вместо
--
-- > mkHMenu def{hmenuText = "Файл", hmenuPopup = popupFile}
-- >         def{hmenuText = "Правка", hmenuPopup = popupEdit}
-- >         def{hmenuText = "Поиск", hmenuPopup = popupFind}
--
-- Можно и
--
-- > V.fromList [def{hmenuText = "Файл", hmenuPopup = popupFile}, ...]
--
mkHMenu :: HMenuMaker r => r
mkHMenu = hmenuMaker V.empty

-- | Параметры горизонтального меню.
-- Хотя параметры состоят только из вектора пунктов меню, он обёрнут в тип для
-- унификации функций создания виджетов.
newtype HorizontalMenuDef = HorizontalMenuDef  { hmenuItems :: V.Vector HMenuItem }

instance Default HorizontalMenuDef where
    def = HorizontalMenuDef { hmenuItems = V.empty
                            }

-- | no exported. Используется внутри horizontalMenu
data Item = Item { itemText :: T.Text
                 , itemX    :: Coord
                 , itemW    :: Coord
                 , itemPopup  :: MenuItems
                 }

-- | Тип созданного горизонтального меню.
data HorizontalMenuData = HorizontalMenuData

-- | Функция создания виджета горизонтального меню.
horizontalMenu :: MonadIO m => HorizontalMenuDef ->  -- ^ Параметры виджета.
                               Widget -> -- ^ Будующий предок в дереве виджетов (обычно @vLayout@).
                               Skin -> -- ^ Skin.
                               m (GuiWidget HorizontalMenuData)
horizontalMenu HorizontalMenuDef{..} parent skin = do
    win <- getWidgetWindow parent
    gui <- getWindowGui win
    items <- newMonadIORef V.empty
    selectedItNum <- newMonadIORef (-1)
    activeItNum <- newMonadIORef (-1)
    fnt <- runProxyCanvas parent $ getFont "menu"
    let recalc guiSt = do
            let visibleItems = V.filter (isVisibleActionState . fromActionMask guiSt . hmenuMask) hmenuItems
            vSz <- V.forM visibleItems (P.textSize fnt . hmenuText)
            writeMonadIORef items $ V.create $ do
                    v <- VM.new $ V.length visibleItems
                    V.ifoldM'_ (\x i (HMenuItem{..},V2 w _) ->
                        let w' = w+2*PaddingX in
                        VM.write v i
                            Item{ itemText = hmenuText, itemX=x, itemW=w', itemPopup=
                                if ActionEnable == fromActionMask guiSt hmenuMask then hmenuPopup
                                else V.empty
                                    } >> return (x+w')) 0 $ V.zip visibleItems vSz
                    return v
            return $ 2*PaddingY + V.foldr (\ (V2 _ h) -> max h) 0 vSz -- Высота меню

        itemEnable = not . V.null . itemPopup
        getItemNumFromX x = do
            v <- readMonadIORef items
            return $ case V.findIndex (\Item{..} -> inRange (itemX,itemX+itemW) x) v of
                        Just i | itemEnable (v V.! i) -> i
                        _ -> (-1)
        setSelected widget i = writeMonadIORef selectedItNum i >> markWidgetForRedraw widget
        setActive widget i = writeMonadIORef activeItNum i >> setSelected widget i
        setNextItem widget i = do
              v <- readMonadIORef items
              let go n | n == i = return ()
                       | n == V.length v = go 0
                       | itemEnable (v V.! n) = doItem widget n
                       | otherwise = go $ n+1
              go $ i+1
        setPrevItem widget i = do
              v <- readMonadIORef items
              let go n | n == i = return ()
                       | n <0 = go $ V.length v - 1
                       | itemEnable (v V.! n) = doItem widget n
                       | otherwise = go $ n-1
              go $ i-1
        doItem widget i = when (i>=0) $ do
            setActive widget i
            setWidgetFocus widget
            curIt <- (V.! i) <$> readMonadIORef items
--            liftIO $ putStrLn $ concat ["horizontalMenu.doItem "] -- -, V.head p]
            (SDL.Rectangle _ (V2 _ h)) <- getWidgetRect widget
            popupMenu widget (SDL.Rectangle (P (V2 (itemX curIt) h)) (V2 10 10)) $ itemPopup curIt

    hmenuHeight <- getGuiState gui >>= recalc
    let hmenuSz = V2 (-1) hmenuHeight
        fns = noChildrenFns hmenuSz
    mkWidget (WidgetVisible .|. WidgetEnable .|. WidgetFocusable) WidgetMarginNone
            HorizontalMenuData parent fns{
        onCreate = \widget -> onCreate fns widget >> setWinMainMenu win (Just widget)
--         onResizing= \widget r -> simpleOnResizing widget r >> notifyParentAboutSize widget hmenuSz
        ,onLostMouseFocus = \widget -> setSelected widget (-1)
        ,onLostKeyboardFocus = \widget -> setActive widget (-1)
        ,onMouseMotion = \widget _btnsLst (P (V2 x _)) _relMv -> do
            n <- getItemNumFromX x
            o <- readMonadIORef selectedItNum
            when (n /= o) (setSelected widget n)
        ,onMouseButton = \widget motion mouseButton _clicks (P (V2 x _)) ->
          when ((motion==SDL.Pressed) && (mouseButton == SDL.ButtonLeft))
            (getItemNumFromX x >>= doItem widget)
        ,onKeyboard = \widget motion _repeated keycode km -> do
--            liftIO $ putStrLn $ concat ["horizontalMenu.onKeyboard "]
            let shiftCtrlAlt = getShftCtrlAlt km
            when (motion==SDL.Pressed && shiftCtrlAlt == ShiftCtrlAlt False False False) $ do
--                                   (isEnterKey keycode || keycode== SDL.KeycodeSpace)) $ -- do
--                liftIO $ putStrLn $ concat ["horizontalMenu.onKeyboard passed"]
                o <- readMonadIORef activeItNum
                when (o >= 0) (case keycode of
                                SDL.KeycodeLeft -> setPrevItem widget o
                                SDL.KeycodeRight -> setNextItem widget o
                                SDL.KeycodeEscape -> clearWidgetFocus widget
                                _ -> return ())
        ,onGuiStateChange = \ widget newState -> recalc newState >> setActive widget (-1)
        ,onNotify = \ _widget _notifyCode _mbSrcWidget -> return ()
        ,onDraw= \widget -> do
                v <- readMonadIORef items
                nSel <- readMonadIORef selectedItNum
                nActive <- readMonadIORef activeItNum
                let menuBkColor = decoreBkColor (formDecore skin)
                r <- getVisibleRect widget
                setColor menuBkColor
                fillRect r
                (`V.imapM_` v) $ \ i it@Item{..} -> do
                    let d | i==nActive = btnDecoreFocused
                          | i==nSel = btnDecoreIn
                          | itemEnable it = btnDecoreOut
                          | otherwise = btnDecoreDisabled
                    bkClr <- if i==nActive || i==nSel then do
                                let ir = SDL.Rectangle (P(V2 itemX 0)) (V2 itemW hmenuHeight)
                                    c = decoreBkColor $ d $ formItemsButtons skin
                                setColor c >> fillRect ir >> return c
                              else return menuBkColor
                    drawTextOpaque fnt (decoreFgColor $ d $ formItemsButtons skin) bkClr
                        (P (V2 (itemX+PaddingX) PaddingY)) itemText
                                                            }