{-# LANGUAGE MultiWayIf #-}

module Main where

import VC.Update.Prelude
import VC.Update.Class
import VC.Update.App
import Data.SL
import System.Exit
import Options as O
import Data.List.Extra as L

main :: IO ()
main = O.runCommand $ \opts configPath -> do 
   env' <- load $ head configPath
   license' <- L.trim <$> (readFile $ licenseFile env')
   version' <- L.trim <$> (readFile $ versionFile env')
   writeLog $ printf "Current version: %s" version'
   let env = env' {
      mainOptions = pure opts,
      license = pure license',
      version = pure version'
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
   when (isJust pid) $ liftIO $ do
      let cmd = if isWindows 
                  then printf "taskkill /pid /t %d" $ fromJust pid
                  else printf "kill -15 %d" $ fromJust pid
      writeLog $ printf "Shutdown UI with %s" cmd
      callCommand cmd
   appUpdate
   when (optInstall options) $ liftIO $ void $ do
      installer'' <- makeAbsolute installer' 
      pid <- getProcessID
      let args = ["--kill", show pid]
      writeLog $ printf "Install with %s" $ showCommandForUser installer'' args
      spawnProcess installer'' args
      
