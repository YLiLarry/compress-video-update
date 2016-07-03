module VC.Update.Prelude (module X, writeLog) where

import Control.Monad.State as X
import GHC.Fingerprint as X
import Debug.Trace as X
import Control.Monad.Extra as X
import Data.Monoid as X
import Data.String as X
import System.IO as X
import System.Directory as X
import Network.HTTP.Simple as X
import Text.Printf as X
import Codec.Archive.Zip as X
import Control.Monad.Extra as X
import Data.Maybe as X
import System.Info.Extra as X
import System.Process as X

writeLog :: (MonadIO m) => String -> m ()
writeLog s = liftIO $ hPutStrLn stderr s

