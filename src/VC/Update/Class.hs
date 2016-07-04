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
import Options

data EnvCfg = EnvCfg {
     serverConfigFingerprintURL :: String
   , serverConfigDirectoryURL :: String
   , serverReleaseVersionURL :: String
   , serverReleaseDownloadURL :: String
   , localConfigDirectoryURL :: String
   , appDir :: FilePath
   , version :: String
   , licenseFile :: FilePath
   , license :: Maybe String
   , envCfg :: FilePath
   , mainOptions :: MainOptions
   , appName :: String
} deriving (Generic)


data MainOptions = MainOptions {
   optCheckOnly :: Bool,
   optKillThread :: Maybe Int,
   optLaunchApp :: Bool
} deriving (Generic)

instance ToJSON EnvCfg
instance FromJSON EnvCfg
instance SL EnvCfg

instance ToJSON MainOptions
instance FromJSON MainOptions
instance SL MainOptions

instance Options MainOptions where
   defineOptions = 
      pure MainOptions
        <*> simpleOption "check-only" False
            "Check if an update is needed. Exit with 0 if not."
        <*> simpleOption "kill-thread" Nothing
            "Kill thread # in order to update."
        <*> simpleOption "launch-app" False
            "Launch the app after update."
            
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
   code <- license <$> get
   return
      $ setRequestBodyURLEncoded inputs
      $ setRequestMethod "POST"
      $ setRequestQueryString [("activation", fromString <$> code)]
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

instance PrintfArg L.ByteString where
   formatArg = formatArg . L.unpack
   