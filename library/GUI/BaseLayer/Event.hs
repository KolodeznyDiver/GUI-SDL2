--  {-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module GUI.BaseLayer.Event(
    GuiPipeProducer,GuiPipe(..),userEventHandler,getPipeIdFromProducer,delGuiPipe
    ) where

import Foreign
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
--import qualified Data.Vector.Storable as V
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.IntMap.Strict as IntMap
import Maybes (whenIsJust)
import qualified SDL
import qualified SDL.Raw as Raw
import GUI.BaseLayer.Internal.Types
import GUI.BaseLayer.Ref
import GUI.BaseLayer.Utils

data GuiPipeProducer a = GuiPipeProducer  { -- guiPipeWinId :: GuiWindowId
                                            guiPipeEventCode :: Word32
                                          , guiPipeId :: Int32
                                          }

class GuiPipe a b | a -> b where
    newGuiPipe :: forall m. MonadIO m => Gui -> (forall n. MonadIO n => GuiPipeId -> a -> n ()) ->
                    m (GuiPipeProducer b)
    newGuiPipe gui f = do
        GUIStruct{..} <- readMonadIORef gui
        pipeId <- getNewGuiPipeId gui
        let fn p0 p1 = f (GuiPipeId pipeId) =<< liftIO (guiPipeDecoder p0 p1)
            n = IntMap.insert pipeId (UserMsgHandler fn) guiUserMsgHandlers
        modifyMonadIORef' gui (\x -> x{guiUserMsgHandlers=n})
        return $ GuiPipeProducer userEventCodeBase (fromIntegral pipeId)

    replaceGuiPipeHandler :: forall m. MonadIO m => Gui -> GuiPipeProducer b ->
                    (forall n. MonadIO n => GuiPipeId -> a -> n ()) -> m ()
    replaceGuiPipeHandler gui prod f = do
        GUIStruct{..} <- readMonadIORef gui
        let pipeId = fromIntegral $ guiPipeId prod
            fn p0 p1 = f (GuiPipeId pipeId) =<< liftIO (guiPipeDecoder p0 p1)
            n = IntMap.adjust (const (UserMsgHandler fn)) pipeId guiUserMsgHandlers
        modifyMonadIORef' gui (\x -> x{guiUserMsgHandlers=n})

    sendToGuiPipe :: forall m. MonadIO m => GuiPipeProducer b -> a -> m Bool
    sendToGuiPipe GuiPipeProducer{..} a = do
        (p0,p1) <- liftIO $ guiPipeEncoder a
        timestamp <- SDL.ticks
        result <- (1==) <$> liftIO (with (Raw.UserEvent guiPipeEventCode timestamp 0 -- winId
                                        guiPipeId p0 p1) Raw.pushEvent)
        unless result $ liftIO $ freeMessage a p0 p1
        return result

    guiPipeEncoder :: a -> IO (Ptr (), Ptr ())
    guiPipeDecoder :: Ptr () -> Ptr () -> IO a
    freeMessage  :: a -> Ptr () -> Ptr () -> IO ()

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


userEventHandler :: MonadIO m => Gui -> Int32 -> Ptr () -> Ptr () -> m ()
userEventHandler gui code p0 p1 = do
    intMap <- guiUserMsgHandlers <$> readMonadIORef gui
    whenIsJust (IntMap.lookup (fromIntegral code) intMap) $ \h -> do
        userMsgHandler h p0 p1
        redrawAll gui

getPipeIdFromProducer :: GuiPipeProducer a -> GuiPipeId
getPipeIdFromProducer = GuiPipeId . fromIntegral . guiPipeId
{-# INLINE getPipeIdFromProducer #-}

delGuiPipe :: MonadIO m => Gui -> GuiPipeProducer a -> m ()
delGuiPipe gui prod = do
    GUIStruct{..} <- readMonadIORef gui
    let pipeId = fromIntegral $ guiPipeId prod
    when (IntMap.member pipeId guiUserMsgHandlers) $
        modifyMonadIORef' gui (\x -> x{guiUserMsgHandlers=IntMap.delete pipeId guiUserMsgHandlers,
                                        guiUserMsgRemovedIds= VU.snoc guiUserMsgRemovedIds pipeId})

-- no export
getNewGuiPipeId :: MonadIO m => Gui -> m Int
getNewGuiPipeId gui = do
    GUIStruct{..} <- readMonadIORef gui
    if VU.null guiUserMsgRemovedIds then
        if IntMap.null guiUserMsgHandlers then return 0
        else return $ succ $ fst $ IntMap.findMax guiUserMsgHandlers
    else let (vHead,vTail) = VU.splitAt 1 guiUserMsgRemovedIds in
         modifyMonadIORef' gui (\x -> x{guiUserMsgRemovedIds=vTail}) >> return (VU.head vHead)
{-# INLINE getNewGuiPipeId #-}
