-- |
-- Module:      Control.Monad.Auxiliaries
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Вспомогательные функции использующие монады.

module Control.Monad.Auxiliaries where


onJustM :: Monad m => Maybe a -> (a -> m (Maybe b)) -> m (Maybe b)
onJustM (Just a) f = f a
onJustM Nothing  _ = return Nothing

