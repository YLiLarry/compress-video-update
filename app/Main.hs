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
   license' <- readFile $ licenseFile env'
   progName <- getProgName
   let env = env' {
      mainOptions = pure opts,
      license = pure license',
      selfName = pure progName
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
   env <- get
   let options = mainOptions ? env
   let installer' = installer ? env
   let pid = optKillProcess options 
   when (isJust pid) $ liftIO $
      if isWindows 
         then callCommand $ printf "taskkill /pid /t %d" $ fromJust pid
         else callCommand $ printf "pkill -P %d" $ fromJust pid
   appUpdate
   when (isJust $ optKillProcess options) $ liftIO $ void $ do
      installer'' <- makeAbsolute installer' 
      spawnProcess installer'' ["--kill", env # selfName]
      
   
   