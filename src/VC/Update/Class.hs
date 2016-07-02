{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module VC.Update.Class where

import VC.Update.Prelude

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import qualified Network.HTTP.Simple as N
import Data.Aeson (FromJSON)

data EnvCfg = EnvCfg {
     serverConfigFingerprintURL :: String
   , serverConfigDirectoryURL :: String
   , serverReleaseVersionURL :: String
   , localConfigDirectoryURL :: String
   , version :: String
   , activation :: String
}


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


requestJSON :: (FromJSON x) => String -> [(S.ByteString, S.ByteString)] -> VCUpdate x
requestJSON url inputs = do 
   req <- request url inputs
   getResponseBody <$> httpJSON req
   
