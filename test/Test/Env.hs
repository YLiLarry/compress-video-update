module Test.Env where

import VC.Update.Prelude
import VC.Update.Class

testEnvCfg :: EnvCfg
testEnvCfg = EnvCfg {
     serverConfigFingerprintURL = "http://localhost:8000/api/config/fingerprints"
   , serverConfigDirectoryURL   = "http://localhost:8000/api/config/"
   , serverReleaseVersionURL    = "http://localhost:8000/api/release/version"
   , localConfigDirectoryURL    = "./test/tmp/"
   , version = "current"
   , activation = "test"
}
