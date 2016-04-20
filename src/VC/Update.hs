{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module VC.Update where

import Network.HTTP.Simple
import Control.Monad.Trans.Except
import System.Directory
import GHC.Fingerprint
import Debug.Trace
import Control.Monad
-- import Data.List
import Data.Monoid
import Data.String
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.ByteString.Lazy (ByteString)
import           Data.Set (Set)
import qualified Data.Set as S ((\\), union, fromList, map)
import System.IO

data URLConfig = URLConfig {
     serverConfigFingerprintURL :: String
   , serverConfigDirectoryURL :: String
   , localConfigDirectoryURL :: String
}

type ConfigName = FilePath

serverConfigFingerprints :: ReaderT URLConfig IO [(ConfigName, String)]
serverConfigFingerprints = do
   request <- fromString <$> serverConfigFingerprintURL <$> ask
   getResponseBody <$> httpJSON request

localConfigFingerprints :: ReaderT URLConfig IO [(ConfigName, String)]
localConfigFingerprints = do   
   path <- localConfigDirectoryURL <$> ask
   files <- liftIO $ getDirectoryContents path
   sequence [ make path f | f <- files, notElem f [".", ".."] ]
      where
      make p f = do
         p <- liftIO $ show <$> getFileHash (p ++ f)
         return (f, p)         

serverLocalDiff :: ReaderT URLConfig IO (Set ConfigName, Set ConfigName)
serverLocalDiff = do
   server <- S.fromList . map fst <$> serverConfigFingerprints
   local  <- S.fromList . map fst <$> localConfigFingerprints
   let u = S.union server local
   return (u S.\\ server, u S.\\ local)

downloadServerConfig :: ConfigName -> ReaderT URLConfig IO ByteString
downloadServerConfig target = do
   file <- mappend <$> (serverConfigDirectoryURL <$> ask) <*> return target
   liftIO $ do
      hPutStrLn stderr $ "Downloading " ++ file
      getResponseBody <$> httpLBS (fromString file)

deleteLocalConfig :: ConfigName -> ReaderT URLConfig IO ()
deleteLocalConfig target = do
   base <- localConfigDirectoryURL <$> ask
   liftIO $ removeFile (base ++ target)

updateAll :: ReaderT URLConfig IO ()
updateAll = do
   (toDelete, toDownload) <- serverLocalDiff 
   mapM_ deleteLocalConfig toDelete
   mapM_ downloadServerConfig toDownload
   
   
