{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      GUI.BaseLayer.Pipe
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <kolodeznydiver@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Потокобезопасные каналы ('GuiPipe') для реализации которых используется очередь сообщений SDL,
-- т.е. получение сообщения \"разбудит\" GUI ожидающий SDL сообщения.

module GUI.BaseLayer.Pipe(
    GuiPipeId,GuiPipeProducer,GuiPipe(..),userMsgHandler,getPipeIdFromProducer,delGuiPipe,mkGuiPipes
    ) where

import Foreign
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.IntMap.Strict as IntMap
import Control.Monad.Extra (whenJust)
import qualified SDL
import qualified SDL.Raw as Raw
import GUI.BaseLayer.Depend0.Pipe
import GUI.BaseLayer.Depend0.Ref
import GUI.BaseLayer.Types
import GUI.BaseLayer.GUIRecord
--import GUI.BaseLayer.Core
import GUI.BaseLayer.RedrawWindow

-- | Уникальный идентификатор канала. Создаётся при создании канала.
newtype GuiPipeId = GuiPipeId Int
            deriving (Eq)

-- | Тип возвращаемый функцией @newGuiPipe@ котрый можно использовать для отправки сообщения к
-- функции обработчику переданному @newGuiPipe@.
data GuiPipeProducer a = GuiPipeProducer  { guiPipeEventCode :: Word32
                                          , guiPipeId :: Int32
                                          }

-- | Реализация канала передачи данных типа __/a/__ с помощью 'GuiPipeProducer' __/b/__.
class GuiPipe a b | a -> b where
    -- | Создаёт новый канал.
    newGuiPipe :: forall m. MonadIO m =>
        Gui -> -- ^ Ссылка на GUI приложения.
        (forall n. MonadIO n => GuiPipeId -> a -> n ()) -> -- ^ Функция принимающая сообщения из канала.
        m (GuiPipeProducer b)
    newGuiPipe gui f = do
        GUIRecord{..} <- readMonadIORef gui
        pipeId <- getNewGuiPipeId gui
        let fn p0 p1 = f (GuiPipeId pipeId) =<< liftIO (guiPipeDecoder p0 p1)
        setHndlrs gui $ IntMap.insert pipeId (UserMsgHandler fn) $ hndlrs guiPipes
        return $ GuiPipeProducer userEventCodeBase (fromIntegral pipeId)

    -- | Заменить обработчик существующего канала идентифицируемого через его 'GuiPipeProducer'.
    replaceGuiPipeHandler :: forall m. MonadIO m =>
        Gui -> -- ^ Ссылка на GUI приложения.
        GuiPipeProducer b -> -- ^ Заменяемый канал, обязательно с данными того же типа.
        (forall n. MonadIO n => GuiPipeId -> a -> n ()) -> -- ^ Новая функция принимающая сообщения из канала.
        m ()
    replaceGuiPipeHandler gui prod f = do
        GUIRecord{..} <- readMonadIORef gui
        let pipeId = fromIntegral $ guiPipeId prod
            fn p0 p1 = f (GuiPipeId pipeId) =<< liftIO (guiPipeDecoder p0 p1)
        setHndlrs gui $ IntMap.adjust (const (UserMsgHandler fn)) pipeId $ hndlrs guiPipes

    -- | Послать сообщение в канал. Возможно из thread.
    -- Функция не ожидает завершения прохождения сообщения по каналу.
    sendToGuiPipe :: forall m. MonadIO m =>
        GuiPipeProducer b -> -- ^ Идентификатор канала.
        a -> -- ^ Посылаемые данные.
        m Bool  -- ^ Успешно или нет.
    sendToGuiPipe GuiPipeProducer{..} a = do
        (p0,p1) <- liftIO $ guiPipeEncoder a
        timestamp <- SDL.ticks
        result <- (1==) <$> liftIO (with (Raw.UserEvent guiPipeEventCode timestamp 0 -- winId
                                        guiPipeId p0 p1) Raw.pushEvent)
        unless result $ liftIO $ freeMessage a p0 p1
        return result

    -- | Кодировщик данных сообщения в два указателя которые будут переданы в сообщении SDL.
    -- Может выделять память и другие ресурсы которые потом должны быть освобождаться с помощью @freeMessage@.
    guiPipeEncoder :: a -> IO (Ptr (), Ptr ())

    -- | Декодировщик данных сообщения из двух указателей которые были переданы в сообщении SDL.
    guiPipeDecoder :: Ptr () -> Ptr () -> IO a

    -- | Функция освобождения памяти и других возможных ресурсов выделенных в @guiPipeEncoder@.
    freeMessage  :: a -> Ptr () -> Ptr () -> IO ()

-- | Реализация 'GuiPipe' для вектора.
instance Storable a => GuiPipe (V.Vector a) a where
    guiPipeEncoder v =
        let cItem = V.length v in
        if cItem == 0 then return (nullPtr,intPtrToPtr $ toEnum 0) else do
           p <- mallocArray cItem
           V.foldM'_ (\i x -> pokeElemOff p i x >> return (i+1)) 0 v
           return (castPtr p,intPtrToPtr $ toEnum cItem)

    guiPipeDecoder p0 p1 =  let cItem = fromEnum $ ptrToIntPtr p1 in
                            if p0 == nullPtr || cItem<=0 then return V.empty else do
                                let p = castPtr p0
                                result <-V.generateM cItem (peekElemOff p)
                                freeMessage result p0 p1
                                return result

    freeMessage _ p _ = when (p/=nullPtr) $ free p

-- | Функция вызывается в @GUI.BaseLayer.RunGUI.onEvent@ при получении пользовательского SDL сообщения.
-- Она идентифицирует канал, находит соответствующий обработчик и вызывает его.
userMsgHandler :: MonadIO m => Gui -> Int32 -> Ptr () -> Ptr () -> m ()
userMsgHandler gui code p0 p1 = do
    intMap <- (hndlrs . guiPipes) <$> readMonadIORef gui
    whenJust (IntMap.lookup (fromIntegral code) intMap) $ \h -> do
        logOnErr gui "userMsgHandler" $ usrMsgHandler h p0 p1
        redrawAll gui

-- | Получить уникальный идентификатор канала.
-- Может потребоваться если одна и та же функция будет установлена в качесте обработчика
-- сообщений из канала. Функция обработчик, в качестве первого аргумента получает идентификатор канала,
-- и, таки мобразом, можно различать из какого канала пришло сообщение.
-- В большинстве случаев идентификатор канала в пользовательском коде не нужен.
getPipeIdFromProducer :: GuiPipeProducer a -> GuiPipeId
getPipeIdFromProducer = GuiPipeId . fromIntegral . guiPipeId
{-# INLINE getPipeIdFromProducer #-}

-- | Удалить канал. Новый канал после удаления канала можно создавать только когда есть уверенность
-- что данные уже не посылаются в удалённый канал.
delGuiPipe :: MonadIO m => Gui -> GuiPipeProducer a -> m ()
delGuiPipe gui prod = do
    GUIRecord{..} <- readMonadIORef gui
    let pipeId = fromIntegral $ guiPipeId prod
    when (IntMap.member pipeId $ hndlrs guiPipes) $
        modifyMonadIORef' gui (\x -> x{guiPipes=GuiPipes (IntMap.delete pipeId $ hndlrs guiPipes)
                                                         (VU.snoc (removedIds guiPipes) pipeId)})

-- | Создать данные для управления каналами. Вызывается из @GUI.BaseLayer.RunGUI.runGUI@
mkGuiPipes :: GuiPipes
mkGuiPipes = GuiPipes IntMap.empty VU.empty
{-# INLINE mkGuiPipes #-}

-- No exported.
setHndlrs :: forall m. MonadIO m => Gui -> GuiPipeCollection -> m ()
setHndlrs gui n = modifyMonadIORef' gui (\x -> x{guiPipes=GuiPipes n (removedIds $ guiPipes x)})
{-# INLINE setHndlrs #-}

-- No exported.
setRemovedIds :: forall m. MonadIO m => Gui -> VU.Vector Int -> m ()
setRemovedIds gui n = modifyMonadIORef' gui (\x -> x{guiPipes=GuiPipes (hndlrs $ guiPipes x) n})
{-# INLINE setRemovedIds #-}

-- No exported.
getNewGuiPipeId :: MonadIO m => Gui -> m Int
getNewGuiPipeId gui = do
    GUIRecord{..} <- readMonadIORef gui
    if VU.null $ removedIds guiPipes then
        if IntMap.null $ hndlrs guiPipes then return 0
        else return $ succ $ fst $ IntMap.findMax $ hndlrs guiPipes
    else let (vHead,vTail) = VU.splitAt 1 $ removedIds guiPipes in
         setRemovedIds gui vTail >> return (VU.head vHead)
