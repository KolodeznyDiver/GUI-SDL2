module System.X11.Utils(
    hideConsole,withRemoveFromTaskbar
    ) where

import Control.Monad.IO.Class

hideConsole:: IO ()
hideConsole = return ()

withRemoveFromTaskbar :: MonadIO m => String -> m a -> m a
withRemoveFromTaskbar _title f = f
