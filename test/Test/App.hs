module Test.App (test) where
   
import Test.Hspec
import Test.Env
import VC.Update
import VC.Update.Prelude
import VC.Update.Class
import VC.Update.App


test :: IO ()
test = hspec $ do
   describe "Update application from the server." $ do
      it "appNeedsUpdate" $ do
         b <- runVCUpdate appNeedsUpdate testEnvCfg
         b `shouldBe` False
         
         
