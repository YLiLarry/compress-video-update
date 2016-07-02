module VC.Update.App where

import VC.Update.Prelude
import VC.Update.Class

appNeedsUpdate :: VCUpdate Bool
appNeedsUpdate = do
   v <- version <$> get
   url <- serverReleaseVersionURL <$> get
   v' <- requestS url []
   liftIO $ writeLog $ printf "Application needs an update:\n  Current: %s\n  Server:  %s" v v'
   return (fromString v == v')
   
appUpdate :: VCUpdate ()
appUpdate = do
   vurl <- serverReleaseVersionURL <$> get
   version' <- requestS vurl []
   appDir' <- appDir <$> get
   writeLog $ printf "Download release.zip (%s) from the server" version'
   url <- serverReleaseDownloadURL <$> get
   zip <- requestLBS url []
   -- writeLog $ printf "Clean old app in %s" appDir'
   -- liftIO $ whenM (doesDirectoryExist appDir') (removeDirectoryRecursive appDir')
   liftIO $ createDirectoryIfMissing True appDir'
   writeLog $ printf "Unzip to %s" appDir'
   liftIO $ extractFilesFromArchive [OptDestination appDir', OptVerbose] $ toArchive zip
   writeLog $ printf "Update the version number to %s" version'
   modify (\e -> e {version = version'}) 
   writeLog $ printf "Update succeeded."
   