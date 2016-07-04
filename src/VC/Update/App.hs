module VC.Update.App where

import VC.Update.Prelude
import VC.Update.Class

appNeedsUpdate :: VCUpdate Bool
appNeedsUpdate = do
   v <- version <$> get
   url <- (# releaseVersionURL) <$> get
   v' <- requestS url []
   let bool = fromString v /= v'
   when bool $ writeLog $ printf "Application needs an update:\n  Current: %s\n  Server:  %s" v v'
   return bool
   
appUpdate :: VCUpdate ()
appUpdate = do
   vurl <- (# releaseVersionURL) <$> get
   version' <- requestS vurl []
   downloadDir' <- (# downloadDir) <$> get
   writeLog $ printf "Download release.zip (%s) from the server" version'
   url <- (# releaseDownloadURL) <$> get
   zip <- requestLBS url []
   -- writeLog $ printf "Clean old app in %s" downloadDir'
   -- liftIO $ whenM (doesDirectoryExist downloadDir') (removeDirectoryRecursive downloadDir')
   liftIO $ createDirectoryIfMissing True downloadDir'
   writeLog $ printf "Unzip to %s" downloadDir'
   liftIO $ extractFilesFromArchive [OptDestination downloadDir', OptVerbose] $ toArchive zip
   writeLog $ printf "Update the version number to %s" version'
   modify (\e -> e {version = version'}) 
   writeLog $ printf "Update succeeded."
   