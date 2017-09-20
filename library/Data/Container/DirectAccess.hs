{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module:      Data.Container.DirectAccess
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Класс для некоего упорядоченного контейнера с произвольным доступом к элементам под монадой MonadIO
-- и его реализации для некоторых контейнерных типов.

module Data.Container.DirectAccess(
    DAROContainer(..)
    ) where

import Control.Monad.IO.Class
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.IORef


-- | (D)irect (A)ccess (R)ead(o)nly container.
class DAROContainer c v | c -> v where
    sizeDARO :: MonadIO m => c -> m Int
    getItemDARO :: MonadIO m => c -> Int -> m v

instance DAROContainer (V.Vector a) a where
    {-# INLINE sizeDARO #-}
    sizeDARO = return . V.length

    {-# INLINE getItemDARO #-}
    getItemDARO c ix = return $ c V.! ix

instance DAROContainer (VM.IOVector a) a where
    {-# INLINE sizeDARO #-}
    sizeDARO = return . VM.length

    {-# INLINE getItemDARO #-}
    getItemDARO c = liftIO . VM.read c

instance VU.Unbox a => DAROContainer (VU.Vector a) a where
    {-# INLINE sizeDARO #-}
    sizeDARO = return . VU.length

    {-# INLINE getItemDARO #-}
    getItemDARO c ix = return $ c VU.! ix

instance VUM.Unbox a => DAROContainer (VUM.IOVector a) a where
    {-# INLINE sizeDARO #-}
    sizeDARO = return . VUM.length

    {-# INLINE getItemDARO #-}
    getItemDARO c = liftIO . VUM.read c

instance DAROContainer (IORef (V.Vector a)) a where
    {-# INLINEABLE sizeDARO #-}
    sizeDARO = fmap V.length . liftIO . readIORef

    {-# INLINEABLE getItemDARO #-}
    getItemDARO c ix = (V.! ix) <$> liftIO (readIORef c)

instance DAROContainer (IORef (VM.IOVector a)) a where
    {-# INLINEABLE sizeDARO #-}
    sizeDARO = fmap VM.length . liftIO . readIORef

    {-# INLINEABLE getItemDARO #-}
    getItemDARO c ix = liftIO ((`VM.read` ix) =<< readIORef c)

instance VU.Unbox a => DAROContainer (IORef (VU.Vector a)) a where
    {-# INLINEABLE sizeDARO #-}
    sizeDARO = fmap VU.length . liftIO . readIORef

    {-# INLINEABLE getItemDARO #-}
    getItemDARO c ix = (VU.! ix) <$> liftIO (readIORef c)

instance VUM.Unbox a => DAROContainer (IORef (VUM.IOVector a)) a where
    {-# INLINEABLE sizeDARO #-}
    sizeDARO = fmap VUM.length . liftIO . readIORef

    {-# INLINEABLE getItemDARO #-}
    getItemDARO c ix = liftIO ((`VUM.read` ix) =<< readIORef c)

