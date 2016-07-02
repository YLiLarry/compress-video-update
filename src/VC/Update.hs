{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module VC.Update where

import VC.Update.Prelude
import VC.Update.Class

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import           Data.Set (Set)
import qualified Data.Set as S


serverConfigFingerprints :: VCUpdate [(FilePath, String)]
serverConfigFingerprints = do
   url <- serverConfigFingerprintURL <$> get
   writeLog $ "Retrive fingerprints from " ++ url
   requestJSON url []

localConfigFingerprints :: VCUpdate [(FilePath, String)]
localConfigFingerprints = do   
   path <- localConfigDirectoryURL <$> get
   files <- liftIO $ getDirectoryContents path
   sequence [ make path f | f <- files, notElem f [".", ".."] ]
      where
      make p f = do
         p <- liftIO $ show <$> getFileHash (p ++ f)
         return (f, p)         

serverLocalDiff :: VCUpdate (Set FilePath, Set FilePath)
serverLocalDiff = do
   server <- S.fromList <$> serverConfigFingerprints
   local  <- S.fromList <$> localConfigFingerprints
   let u = S.union server local
   return (S.map fst $ u S.\\ server, S.map fst $ u S.\\ local)

downloadServerConfig :: FilePath -> VCUpdate ()
downloadServerConfig target = do
   upstream <- mappend <$> (serverConfigDirectoryURL <$> get) <*> return target
   local <- mappend <$> (localConfigDirectoryURL <$> get) <*> return target
   writeLog $ "Downloading " ++ upstream
   content <- requestLBS upstream []
   writeLog $ "Saved to " ++ local
   liftIO $ B.writeFile local content

deleteLocalConfig :: FilePath -> VCUpdate ()
deleteLocalConfig target = do
   base <- localConfigDirectoryURL <$> get
   liftIO $ removeFile (base ++ target)

updateAll :: VCUpdate ()
updateAll = do
   (toDelete, toDownload) <- serverLocalDiff 
   mapM_ deleteLocalConfig toDelete
   mapM_ downloadServerConfig toDownload
   
   
