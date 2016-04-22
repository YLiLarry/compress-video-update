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
import System.IO
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import           Data.Set (Set)
import qualified Data.Set as S
import VC.Update.Types

writeLog :: MonadIO m => String -> m ()
writeLog str = liftIO $ hPutStrLn stderr str

requestSettings :: String -> ReaderT EnvCfg IO Request
requestSettings url = settings <$> ask
   where settings cfg =
            setRequestMethod "POST" $
            setRequestQueryString [("activation", Just $ fromString $ activation cfg)] $
            fromString url                      

serverConfigFingerprints :: ReaderT EnvCfg IO [(ConfigName, String)]
serverConfigFingerprints = do
   url <- serverConfigFingerprintURL <$> ask
   request <- requestSettings url
   writeLog $ "Retrive fingerprints from " ++ url
   getResponseBody <$> httpJSON request

localConfigFingerprints :: ReaderT EnvCfg IO [(ConfigName, String)]
localConfigFingerprints = do   
   path <- localConfigDirectoryURL <$> ask
   files <- liftIO $ getDirectoryContents path
   sequence [ make path f | f <- files, notElem f [".", ".."] ]
      where
      make p f = do
         p <- liftIO $ show <$> getFileHash (p ++ f)
         return (f, p)         

serverLocalDiff :: ReaderT EnvCfg IO (Set ConfigName, Set ConfigName)
serverLocalDiff = do
   server <- S.fromList <$> serverConfigFingerprints
   local  <- S.fromList <$> localConfigFingerprints
   let u = S.union server local
   return (S.map fst $ u S.\\ server, S.map fst $ u S.\\ local)

downloadServerConfig :: ConfigName -> ReaderT EnvCfg IO ()
downloadServerConfig target = do
   upstream <- mappend <$> (serverConfigDirectoryURL <$> ask) <*> return target
   local <- mappend <$> (localConfigDirectoryURL <$> ask) <*> return target
   request <- requestSettings upstream
   liftIO $ do
      writeLog $ "Downloading " ++ upstream
      content <- getResponseBody <$> httpLBS request
      writeLog $ "Saved to " ++ local
      B.writeFile local content

deleteLocalConfig :: ConfigName -> ReaderT EnvCfg IO ()
deleteLocalConfig target = do
   base <- localConfigDirectoryURL <$> ask
   liftIO $ removeFile (base ++ target)

updateAll :: ReaderT EnvCfg IO ()
updateAll = do
   (toDelete, toDownload) <- serverLocalDiff 
   mapM_ deleteLocalConfig toDelete
   mapM_ downloadServerConfig toDownload
   
   
