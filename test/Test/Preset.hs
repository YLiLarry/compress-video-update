module Test.Preset (test) where

import Test.Hspec
import Test.Env
import VC.Update
import VC.Update.Prelude
import VC.Update.Class

test :: IO ()
test = hspec $ do
   describe "Update user config files from the server." $ do
      it "updateAll" $ do
         createDirectoryIfMissing True $ localConfigDirectoryURL testEnvCfg
         runVCUpdate updateAll testEnvCfg
         content <- readFile $ localConfigDirectoryURL testEnvCfg ++ "测试.cfg"
         content `shouldBe` "test-content"
         removeDirectoryRecursive $ localConfigDirectoryURL testEnvCfg
         
         
