{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
-- |
-- Module:      GUI.Widget.PathBox
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Поле отображающее путь в файловой системе разбитый на отдельные элементы и позволяющий выбрать часть пути.
-- Использует @HorizontalLinks@.

module GUI.Widget.PathBox(
    -- * Типы используемые с @pathBox@.
    PathBoxDef(..),PathBoxData
    -- * Функция создания виджета отображающего путь в файловой системе.
    ,pathBox
    ) where

import Data.Bits
import Data.IORef
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Vector as V
import Control.Exception.Safe
import System.Directory
import System.FilePath
import           TextShow (showb)
import Control.Monad.Extra (whenJust,whenM)
import Data.Default
import qualified SDL
import GUI
import GUI.Widget.HorizontalItems
import GUI.Widget.HorizontalLinks
import GUI.Window.MessageBox
import System.FileSystem

pattern RootName :: FilePath; pattern RootName = "(root)"

-- | Начальные настройки виджета.
data PathBoxDef = PathBoxDef {
          pathBoxFormItemDef  :: FormItemWidgetDef -- ^ Общие настройки для всех виджетов для форм,
                                                   -- в настоящий момент только margin's.
        , pathBoxWidth   :: Coord -- ^ Ширина поля. Высота определяется размером шрифта
                                  -- и внутренними полями фиксированного размера.
        , pathBoxFlags   :: WidgetFlags -- ^ Флаги базового виджета.
        , pathBoxCanMkDir :: Bool  -- ^ Добавлять ли кнопку создания директории.
                             }

instance Default PathBoxDef where
    def = PathBoxDef    {
              pathBoxFormItemDef = def
            , pathBoxWidth = -1
            , pathBoxFlags = WidgetVisible .|. WidgetEnable
            , pathBoxCanMkDir = True
                        }

-- | Не экспортируемый тип записи. Хранится по ссылке в 'HorizItsData'.
newtype Handlers = Handlers { hndlrOnChanged :: forall m. MonadIO m => FilePath -> m ()
                            }

-- | Тип созданного виджета. Обычно используется как  @GuiWidget PathBoxData@.
data PathBoxData = PathBoxData  { lnks :: HorizLinksData
                                , refHandlers :: IORef Handlers
                                }

packPath :: FilePath -> LinkVector
packPath = V.map T.pack . V.fromList . fstFix . splitDirectories
    where fstFix [] = []
          fstFix ([c]:xs) | isPathSeparator c = RootName :xs
          fstFix (x:xs) = dropTrailSep x : xs
            where dropTrailSep s | isPathSeparator $ last s = init s
                                 | otherwise = s

unpackPath :: LinkVector -> FilePath
unpackPath = joinPath . fstFix . V.toList . V.map T.unpack
    where fstFix [] = []
          fstFix s@(x:xs) | x == RootName = [pathSeparator]:xs
                          | ':' == last x = (x ++ [pathSeparator]):xs
                          | otherwise = s

-- | Установка и извлечение Пути.
instance ValueProperty (GuiWidget PathBoxData) FilePath where
    setValue w = setValue w{widgetData= lnks $ widgetData w} . packPath
    getValue w = unpackPath <$> getValue w{widgetData= lnks $ widgetData w}

instance Changeable (GuiWidget PathBoxData) FilePath where
    onChanged w a = modifyMonadIORef' (refHandlers $ widgetData w) (\d -> d{hndlrOnChanged= a})

-- | Функция создания виджета отображения и выбора пути.
pathBox :: MonadIO m => PathBoxDef -> -- ^ Параметры виджета.
                         Widget -> -- ^ Будующий предок в дереве виджетов.
                         Skin -> -- ^ Skin.
                         m (GuiWidget PathBoxData)
pathBox PathBoxDef{..} parent skin = do
    widg <- horizLinks def{
                  horizLnFormItemDef = pathBoxFormItemDef
                , horizLnWidth = pathBoxWidth
                , horizLnFlags = pathBoxFlags
                , horizLnFirstSep = T.empty
                , horizLnSep = T.singleton pathSeparator
                , horizLnLastSep = T.empty
                , horizLnOptBtn = if pathBoxCanMkDir then "plus.png" else T.empty
                , horizLnMoveOnUpdate = MoveToLastOnUpdate
                          }  parent skin
    refH <- newMonadIORef Handlers { hndlrOnChanged = \_ -> return () }
    gui <- getGuiFromWidget parent
    let self = widg{widgetData= PathBoxData (widgetData widg) refH}
        onUpdt newPth = setValue self newPth >> readMonadIORef refH >>= (( $ newPth) . hndlrOnChanged)
                            >> markWidgetForRedraw (baseWidget widg)
        copyToClipboard :: MonadIO m => m ()
        copyToClipboard = getValue self >>= SDL.setClipboardText . T.pack
        pasteFromClipboard :: MonadIO m => m ()
        pasteFromClipboard = whenM SDL.hasClipboardText
                                (SDL.getClipboardText >>= onUpdt . T.unpack)
    onClick1 widg $ \ i -> do
        vPath <- getValue widg
        if i == OptBtnIx then do
            newDirCaption <- getT gui "newDir"
            textInput gui newDirCaption def{
                textInputCharFilter = isLegalFileSymbol
              , textInputAccept = \mbNewDirT -> whenJust mbNewDirT $ \newDirT -> do
                    let newPath = unpackPath vPath </> T.unpack newDirT
                    eRes <- liftIO $ tryAny $ createDirectory newPath
                    case eRes of
                        Left e -> say gui MsgBoxError $ showb e
                        _ -> onUpdt newPath
                                            }
        else onUpdt $ unpackPath $ V.take (i+1) vPath
    fns <- getWidgetFns $ baseWidget widg
    setWidgetFns (baseWidget widg) fns{
        onKeyboard = \widget motion repeated keycode km -> when (motion==SDL.Pressed) $ do
--            liftIO $ putStrLn $ concat ["pathBox.onKeyboard "]
            let ShiftCtrlAlt{isShift=isS,isCtrl=isC,isAlt=isA} = getShftCtrlAlt km
            case keycode of
                    SDL.KeycodeC | not isS && isC && not isA -> copyToClipboard
                    SDL.KeycodeInsert | not isS && isC && not isA -> copyToClipboard
                    SDL.KeycodeV | not isS && isC && not isA -> pasteFromClipboard
                    SDL.KeycodeInsert | isS && not isC && not isA -> pasteFromClipboard
                    _ -> onKeyboard fns widget motion repeated keycode km
                                      }
    return self


