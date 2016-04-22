module VC.Update.Types where

data EnvCfg = EnvCfg {
     serverConfigFingerprintURL :: String
   , serverConfigDirectoryURL :: String
   , localConfigDirectoryURL :: String
   , version :: String
   , activation :: String
}

type ConfigName = FilePath
