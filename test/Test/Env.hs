module Test.Env where

import VC.Update.Prelude
import VC.Update.Class
import Options

testEnvCfg :: EnvCfg
testEnvCfg = EnvCfg {
     presetFingerprintURL  = "http://localhost:8000/api/config/fingerprints"
   , presetCollectionURL   = "http://localhost:8000/api/config/"
   , releaseVersionURL     = "http://localhost:8000/api/release/version"
   , releaseDownloadURL    = "http://localhost:8000/api/release/download"
   , presetDir = "./test/tmp/cfgs/"
   , downloadDir = "./test/tmp/app/"
   , installer = "compress-video"
   , licenseFile = "./test/LICENSE"
   , license = Just "test"
   , version = "current"
   , selfName = Nothing
   , mainOptions = Just defaultOptions
}
