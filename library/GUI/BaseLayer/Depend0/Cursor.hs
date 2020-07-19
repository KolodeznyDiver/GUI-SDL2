{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module:      GUI.BaseLayer.Depend0.Cursor
-- Copyright:   (c) 2017-2020 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Дополнение к пакету SDL2. Функции и типы для работы с курсорами.

module GUI.BaseLayer.Depend0.Cursor(
    -- * Пакет SDL2 не экспортирует функцию @createSystemCursor@ (только @Raw.createSystemCursor@) и
    -- не предоставляет доступ к полю __unwrapCursor__ своего типа __Cursor__.
    -- Пришлось определить свой тип и функции работы с ним (частично использован код из пакета SDL2).
     GuiCursor(..),setCursor,freeCursor
    -- * Набор курсоров 'CursorSet'.
    ,CursorIx(..),pattern DefCursorIx,CursorSet(..),freeCursorSet,mkSystemCursorSet,getCursorByIx
                 ) where

import Control.Monad.IO.Class (MonadIO)
import qualified SDL.Raw as Raw
import qualified Data.Text as T

----------------- Начало кода основанного на коде из пакета SDL2. ------------------

newtype GuiCursor = GuiCursor { unwrapGuiCursor :: Raw.Cursor }
    deriving (Eq)

setCursor :: MonadIO m => GuiCursor -> m ()
setCursor = Raw.setCursor . unwrapGuiCursor

freeCursor :: MonadIO m => GuiCursor -> m ()
freeCursor = Raw.freeCursor . unwrapGuiCursor

createSystemCursor :: MonadIO m => Raw.SystemCursor -> m GuiCursor
createSystemCursor n = GuiCursor <$> Raw.createSystemCursor n

----------------- Окончание кода из пакета SDL2. ------------------

-- | Набор курсоров. Например, курсоры операционной системы.
data CursorSet = CursorSet { arrowCursor :: GuiCursor
                           , ibeamCursor :: GuiCursor
                           , waitCursor :: GuiCursor
                           , crosshairCursor :: GuiCursor
                           , waitarrowCursor :: GuiCursor
                           , sizeNWSECursor :: GuiCursor
                           , sizeNESWCursor :: GuiCursor
                           , sizeWECursor :: GuiCursor
                           , sizeNSCursor :: GuiCursor
                           , sizeAllCursor :: GuiCursor
                           , noCursor :: GuiCursor
                           , handCursor :: GuiCursor
                           }

-- | Тип исользуемый как индекс для доступа к полю 'CursorSet'.
data CursorIx = SystemCursorArrow
              | SystemCursorIbeam
              | SystemCursorWait
              | SystemCursorCrossHair
              | SystemCursorWaitArrow
              | SystemCursorSizeNWSE
              | SystemCursorSizeNESW
              | SystemCursorSizeWE
              | SystemCursorSizeNS
              | SystemCursorSizeAll
              | SystemCursorNo
              | SystemCursorHand
              | CursorResourceIx T.Text -- ^ Курсор из ресурса менеджера ресурсов по текстовому ключу.
                    deriving (Eq)

-- | Курсор по умолчанию для GUI виджетов
pattern DefCursorIx :: CursorIx; pattern DefCursorIx = SystemCursorArrow

-- | Создать 'CursorSet' из системных курсоров.
mkSystemCursorSet:: MonadIO m => m CursorSet
mkSystemCursorSet = CursorSet <$> createSystemCursor Raw.SDL_SYSTEM_CURSOR_ARROW
                              <*> createSystemCursor Raw.SDL_SYSTEM_CURSOR_IBEAM
                              <*> createSystemCursor Raw.SDL_SYSTEM_CURSOR_WAIT
                              <*> createSystemCursor Raw.SDL_SYSTEM_CURSOR_CROSSHAIR
                              <*> createSystemCursor Raw.SDL_SYSTEM_CURSOR_WAITARROW
                              <*> createSystemCursor Raw.SDL_SYSTEM_CURSOR_SIZENWSE
                              <*> createSystemCursor Raw.SDL_SYSTEM_CURSOR_SIZENESW
                              <*> createSystemCursor Raw.SDL_SYSTEM_CURSOR_SIZEWE
                              <*> createSystemCursor Raw.SDL_SYSTEM_CURSOR_SIZENS
                              <*> createSystemCursor Raw.SDL_SYSTEM_CURSOR_SIZEALL
                              <*> createSystemCursor Raw.SDL_SYSTEM_CURSOR_NO
                              <*> createSystemCursor Raw.SDL_SYSTEM_CURSOR_HAND

-- | Освободить ресурсы набора курсоров.
freeCursorSet ::  MonadIO m => CursorSet -> m ()
freeCursorSet CursorSet{..} = freeCursor arrowCursor >> freeCursor ibeamCursor >>
    freeCursor waitCursor >> freeCursor crosshairCursor >> freeCursor waitarrowCursor >>
    freeCursor sizeNWSECursor >> freeCursor sizeNESWCursor >> freeCursor sizeWECursor >>
    freeCursor sizeNSCursor >> freeCursor sizeAllCursor >> freeCursor noCursor >>
    freeCursor handCursor

-- | Получить курсор из набора по индексу.
getCursorByIx :: CursorSet -> CursorIx -> GuiCursor
getCursorByIx s SystemCursorArrow     = arrowCursor s
getCursorByIx s SystemCursorIbeam     = ibeamCursor s
getCursorByIx s SystemCursorWait      = waitCursor s
getCursorByIx s SystemCursorCrossHair = crosshairCursor s
getCursorByIx s SystemCursorWaitArrow = waitarrowCursor s
getCursorByIx s SystemCursorSizeNWSE  = sizeNWSECursor s
getCursorByIx s SystemCursorSizeNESW  = sizeNESWCursor s
getCursorByIx s SystemCursorSizeWE    = sizeWECursor s
getCursorByIx s SystemCursorSizeNS    = sizeNSCursor s
getCursorByIx s SystemCursorSizeAll   = sizeAllCursor s
getCursorByIx s SystemCursorNo        = noCursor s
getCursorByIx s SystemCursorHand      = handCursor s
getCursorByIx _ _ = undefined