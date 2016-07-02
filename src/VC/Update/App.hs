module VC.Update.App where

import VC.Update.Prelude
import VC.Update.Class

appNeedsUpdate :: VCUpdate Bool
appNeedsUpdate = do
   v <- version <$> get
   url <- serverReleaseVersionURL <$> get
   v' <- requestLBS url []
   liftIO $ print v'
   return (fromString v == v')
   


