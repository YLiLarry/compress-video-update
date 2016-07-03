{-# LANGUAGE MultiWayIf #-}

module Main where

import VC.Update.Prelude
import VC.Update.Class
import VC.Update.App
import Data.SL
import System.Exit
import Options as O

main :: IO ()
main = O.runCommand $ \opts configPath -> do 
   env' <- load $ head configPath
   let env = env' {
      mainOptions = opts
   }
   flip runVCUpdate env $
      if | optCheckOnly opts -> checkOnly
         | otherwise -> update
   
checkOnly :: VCUpdate ()
checkOnly = do
   writeLog "Only check if an update is needed."
   unlessM appNeedsUpdate 
      (writeLog "Application is up-to-date.")
      
update :: VCUpdate ()
update = do
   options <- mainOptions <$> get
   appName' <- appName <$> get
   let pid = optKillThread options 
   when (isJust pid) $ liftIO $
      if isWindows 
         then callCommand $ printf "taskkill /pid /t %d" $ fromJust pid
         else callCommand $ printf "pkill -P %d" $ fromJust pid
   appUpdate
   when (optLaunchApp options) $ liftIO $
      callCommand appName'
      
   
   