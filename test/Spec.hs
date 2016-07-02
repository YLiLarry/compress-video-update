import Test.Hspec
import Test.App as App
import Test.Preset as Preset

main :: IO ()
main = do
   App.test
   Preset.test
