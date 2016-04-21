import Test.Hspec

import VC.Update
import System.Directory
import Control.Monad.Trans.Reader

testURLConfig = URLConfig {
     serverConfigFingerprintURL = "http://localhost:8000/api/current/config/fingerprints"
   , serverConfigDirectoryURL   = "http://localhost:8000/api/current/config/fingerprints/"
   , localConfigDirectoryURL    = "./test/test-config-dir/"
}

main :: IO ()
main = hspec $ do
   describe "first" $ do
      it "" $ do
         print =<< runReaderT serverLocalDiff testURLConfig
         

