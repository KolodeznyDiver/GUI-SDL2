module GUI.BaseLayer.Ref where

--import qualified SDL
--import SDL.Vect
--import Data.Word
--import GUI.BaseLayer.Types
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