-- |
-- Module:      GUI.BaseLayer.Depend0.Ref
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <kolodeznydiver@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Набор простых функций несколько сокращающих запись операции со ссылками в монаде 'MonadIO'.

module GUI.BaseLayer.Depend0.Ref where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef
-- import Control.Concurrent.STM

newMonadIORef :: MonadIO m => a -> m (IORef a)
newMonadIORef = liftIO . newIORef
{-# INLINE newMonadIORef #-}

readMonadIORef :: MonadIO m => IORef a -> m a
readMonadIORef = liftIO . readIORef
{-# INLINE readMonadIORef #-}

writeMonadIORef :: MonadIO m => IORef a -> a -> m ()
writeMonadIORef r = liftIO . writeIORef r
{-# INLINE writeMonadIORef #-}

modifyMonadIORef' :: MonadIO m => IORef a -> (a -> a) -> m ()
modifyMonadIORef' r = liftIO . modifyIORef' r
{-# INLINE modifyMonadIORef' #-}
{-
atomicallyMonadIO :: MonadIO m => STM a -> m a
atomicallyMonadIO = liftIO . atomically
{-# INLINE atomicallyMonadIO #-}

newTVarMonadIO :: MonadIO m => a -> m (TVar a)
newTVarMonadIO = liftIO . newTVarIO
{-# INLINE newTVarMonadIO #-}

readTVarMonadIO :: MonadIO m => TVar a -> m a
readTVarMonadIO = liftIO . readTVarIO
{-# INLINE readTVarMonadIO #-}
-}