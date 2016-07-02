module VC.Update.App where

import VC.Update.Prelude
import VC.Update.Class

appNeedsUpdate :: VCUpdate Bool
appNeedsUpdate = do
   v <- version <$> get
   url <- serverReleaseVersionURL <$> get
   v' <- requestLBS url []
   liftIO $ writeLog $ printf "Application needs an update:\n  Current: %s\n  Server:  %s" (show v) (show v')
   return (fromString v == v')
   
appUpdate :: VCUpdate ()
appUpdate = do
   appDir' <- appDir <$> get
   writeLog "Download release.zip from the server"
   url <- serverReleaseDownloadURL <$> get
   zip <- requestLBS url []
   -- writeLog $ printf "Clean old app in %s" appDir'
   -- liftIO $ whenM (doesDirectoryExist appDir') (removeDirectoryRecursive appDir')
   liftIO $ createDirectoryIfMissing True appDir'
   writeLog $ printf "Unzip to %s" appDir'
   liftIO $ extractFilesFromArchive [OptDestination appDir', OptVerbose] $ toArchive zip
   