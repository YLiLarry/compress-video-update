{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module VC.Update.Class where

import VC.Update.Prelude

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString as S
import qualified Network.HTTP.Simple as N
import Data.Aeson (FromJSON)
import Data.SL

data EnvCfg = EnvCfg {
     serverConfigFingerprintURL :: String
   , serverConfigDirectoryURL :: String
   , serverReleaseVersionURL :: String
   , serverReleaseDownloadURL :: String
   , localConfigDirectoryURL :: String
   , appDir :: FilePath
   , version :: String
   , activation :: String
   , envCfg :: FilePath
} deriving (Generic)

instance ToJSON EnvCfg
instance FromJSON EnvCfg
instance SL EnvCfg

saveEnvCfg :: VCUpdate ()
saveEnvCfg = do
   env <- get 
   save env $ envCfg env
   

newtype VCUpdate v = VCUpdate {
   unVCUpdate :: StateT EnvCfg IO v
}

runVCUpdate :: VCUpdate a -> EnvCfg -> IO a
runVCUpdate a env = evalStateT (unVCUpdate a) env

deriving instance Functor VCUpdate
deriving instance Applicative VCUpdate
deriving instance Monad VCUpdate
deriving instance MonadIO VCUpdate
deriving instance MonadState EnvCfg VCUpdate


request :: String -> [(S.ByteString, S.ByteString)] -> VCUpdate Request
request url inputs = do
   code <- activation <$> get
   return
      $ setRequestBodyURLEncoded inputs
      $ setRequestMethod "POST"
      $ setRequestQueryString [("activation", Just $ fromString code)]
      $ fromString url
      
requestLBS :: String -> [(S.ByteString, S.ByteString)] -> VCUpdate L.ByteString
requestLBS url inputs = do 
   req <- request url inputs
   getResponseBody <$> httpLBS req

requestS :: String -> [(S.ByteString, S.ByteString)] -> VCUpdate String
requestS url inputs = L.unpack <$> requestLBS url inputs

requestJSON :: (FromJSON x) => String -> [(S.ByteString, S.ByteString)] -> VCUpdate x
requestJSON url inputs = do 
   req <- request url inputs
   getResponseBody <$> httpJSON req
   

writeLog :: MonadIO m => String -> m ()
writeLog str = liftIO $ hPutStrLn stderr str                


instance PrintfArg L.ByteString where
   formatArg = formatArg . L.unpack
   