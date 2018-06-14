{-# LANGUAGE RankNTypes #-}

-- |
-- Module:      GUI.BaseLayer.Depend0.Pipe
-- Copyright:   (c) 2017-2018 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Потокобезопасные каналы для реализации которых используется очередь сообщений SDL.
--
-- Данный модуль описывает типы используемые в "GUI.BaseLayer.Depend0.Pipe" и "GUI.BaseLayer.Types".
-- Типы из этого модуля не предназначены для использования за пределами /GUI.BaseLayer/.

module GUI.BaseLayer.Depend0.Pipe where

import Foreign (Ptr)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Vector.Unboxed as VU
import qualified Data.IntMap.Strict as IntMap


-- | Тип внутреннего обработчика реализации "GUI.BaseLayer.Pipe".
-- Параметры непосредственно соотвествуют пользовательскому сообщению SDL.
newtype UserMsgHandler = UserMsgHandler {usrMsgHandler :: forall m. MonadIO m => Ptr () -> Ptr () -> m ()}

-- | Контейнер хранения активных Обработчиков сообщений.
type GuiPipeCollection = IntMap.IntMap UserMsgHandler

-- | Все активные каналы.
data GuiPipes = GuiPipes {
    hndlrs :: GuiPipeCollection -- ^ Обработчики сообщений передаваемых через
                                -- 'GUI.BaseLayer.Pipe.GuiPipe'.
  , removedIds :: VU.Vector Int -- ^ Идентификаторы удалённых pipes, пригодные к повторному
                                -- использованию.
                         }
