{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
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
     presetFingerprintURL :: String
   , presetCollectionURL :: String
   , releaseVersionURL :: String
   , releaseDownloadURL :: String
   , presetDir :: String
   , installer :: String
   , downloadDir :: FilePath
   , licenseFile :: FilePath
   , license :: Maybe String
   , versionFile :: FilePath
   , version :: Maybe String
   , mainOptions :: Maybe MainOptions
} deriving (Generic)

class MaybeData a
class (MaybeData a) => Selector a f c where
   (#) :: a -> f -> c
   (?) :: f -> a -> c
   (?) = flip (#)
   
instance (MaybeData a) => Selector a (a -> Maybe b) b where
   a # f = fromJust $ f a
instance (MaybeData a) => Selector a (a -> b) b where
   a # f = f a
instance MaybeData EnvCfg

data MainOptions = MainOptions {
   optCheckOnly :: Bool,
   optKillProcess :: Maybe Integer,
   optInstall :: Bool
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
        <*> simpleOption "kill" Nothing
            "Kill process name in order to update."
        <*> simpleOption "install" False
            "Run installer after download."
            
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
   