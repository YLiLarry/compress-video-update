import Test.Hspec

import VC.Update
import VC.Update.Types
import System.Directory
import Control.Monad.Trans.Reader

testEnvCfg = EnvCfg {
     serverConfigFingerprintURL = "http://localhost:8000/api/config/fingerprints"
   , serverConfigDirectoryURL   = "http://localhost:8000/api/config/"
   , localConfigDirectoryURL    = "./test/tmp/"
   , version = "current"
   , activation = "activation-code"
}

main :: IO ()
main = hspec $ do
   describe "Update user config files from the server." $ do
      it "updateAll" $ do
         createDirectoryIfMissing True $ localConfigDirectoryURL testEnvCfg
         runReaderT updateAll testEnvCfg
         content <- readFile $ localConfigDirectoryURL testEnvCfg ++ "测试.cfg"
         content `shouldBe` "test-content"
         removeDirectoryRecursive $ localConfigDirectoryURL testEnvCfg
         
         
