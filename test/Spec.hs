import Test.Hspec

import VC.Update
import System.Directory

testURLConfig = URLConfig {
     serverConfigFingerprintURL = String
   , serverConfigDirectoryURL   = String
   , localConfigDirectoryURL    = "./test/test-config-dir/"
}

main :: IO ()
main = hspec $ do
   describe "first" $ do
      it "" $ do
         print =<< localConfigFingerprints 
         

