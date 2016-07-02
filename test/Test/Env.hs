module Test.Env where

import VC.Update.Prelude
import VC.Update.Class

testEnvCfg :: EnvCfg
testEnvCfg = EnvCfg {
     serverConfigFingerprintURL = "http://localhost:8000/api/config/fingerprints"
   , serverConfigDirectoryURL   = "http://localhost:8000/api/config/"
   , serverReleaseVersionURL    = "http://localhost:8000/api/release/version"
   , serverReleaseDownloadURL    = "http://localhost:8000/api/release/download"
   , localConfigDirectoryURL    = "./test/tmp/cfgs/"
   , appDir = "./test/tmp/app/"
   , version = "current"
   , activation = "test"
}
