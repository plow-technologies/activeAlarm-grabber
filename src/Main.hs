{- |
Module      :  <Main.hs>
Description :  <Executable for activealarmgrabber>
Copyright   :  (c) <Plow Technology 2014>
License     :  <MIT>

Maintainer  :  <lingpo.huang@plowtech.net>
Stability   :  unstable
Portability :  portable 

<Reads configs and pass it to the key matching functions>
-}

{-# LANGUAGE OverloadedStrings #-}

--General
import           Control.Applicative
-- Types
import qualified Data.Aeson               as A
import qualified Data.ByteString.Lazy.Char8     as BSL
import qualified Data.Traversable               as T
import Data.Yaml (encodeFile)
-- Database
import           Database.Persist
import           Persist.Mongo.Settings
-- Internal
import           Active.Alarm.Grabber
import           Active.Alarm.Grabber.Types 

main :: IO ()
main = do
  dbConf <- readDBConf "config.yml"
  genConf <- readKeyGen "config.yml"
  let hosts = (hostList genConf)
  eskrList <- T.sequence $ (\conf -> createAndMatchKeys conf buildQuery getAlarmId hosts) <$> dbConf
  case eskrList of
    Left _ -> putStrLn "Error reading config file"
    Right skrList -> do 
                  encodeFile "singleKeyRouter.yml" (listToOutput skrList)
                  esrcf <- readSingleKeyRouterConfig "singleKeyRouter.yml"
                  case esrcf of
                    Left e -> fail e
                    Right srcf -> do
                              mapM_ (BSL.putStrLn . A.encode) srcf
                              putStrLn "Successfully Write to YAML File."

getAlarmId :: Entity Alarm -> Key Alarm
getAlarmId = entityKey

buildQuery :: [Filter Alarm]
buildQuery = [AlarmAlarmActive ==. True]

