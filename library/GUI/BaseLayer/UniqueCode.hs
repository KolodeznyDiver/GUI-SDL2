-- |
-- Module:      GUI.BaseLayer.UniqueCode
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Поддержка уникального кода в пределах GUI.  Может использоваться в качестве атома.

module GUI.BaseLayer.UniqueCode(
    UniqueCode(..),getUniqueCode
    ) where

import Control.Monad.IO.Class (MonadIO)
import GUI.BaseLayer.Depend0.Ref
import GUI.BaseLayer.Types

-- | Тип - обёртка для уникального кода, который можно получить от @Gui@
-- c помощью @GUI.BaseLayer.Core.getUniqueCode@. Может использоваться в качестве атома.
newtype UniqueCode = UniqueCode { unUniqueCode :: Int}
            deriving (Eq)

-- | Получить следующий уникальный код.
getUniqueCode :: MonadIO m => Gui -> m UniqueCode
getUniqueCode gui = do
    g <- readMonadIORef gui
    return (UniqueCode $ guiUnique g) <* writeMonadIORef gui g{guiUnique= 1 + guiUnique g}
{-# INLINE getUniqueCode #-}



