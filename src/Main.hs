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
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Traversable           as T
import           Data.Yaml                  (encodeFile)
-- Database
import           Database.Persist
import           Persist.Mongo.Settings
-- Internal
import           Active.Alarm.Grabber
import           Active.Alarm.Grabber.Types
import           Data.Text                  (Text, pack)

main :: IO ()
main = do
  dbConf <- readDBConf "config.yml"
  genConf <- readKeyGen "config.yml"
  putStrLn "Welcome to Alarm Key Grabber, Please choose an option"
  putStrLn "1. Grab Active Alarms"
  putStrLn "2. Grab a Site"
  input <- getLine
  case input of
    "1" -> do
           _ <- grabActiveAlarms dbConf genConf
           return ()
    "2" -> do
           putStrLn "Input a Site Name :"
           sitename <- getLine
           _ <- grabSiteAlarms dbConf genConf (pack sitename)
           return ()
    _ -> print ("Not available option."::String)


getAlarmId :: Entity Alarm -> Key Alarm
getAlarmId = entityKey

buildQuery :: [Filter Alarm]
buildQuery = [AlarmAlarmActive ==. True]

grabActiveAlarms :: Either t MongoDBConf -> KeyGenConfig -> IO ()
grabActiveAlarms  dbConf genConf = do
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

grabSiteAlarms :: Either t MongoDBConf -> KeyGenConfig -> Text ->  IO ()
grabSiteAlarms  dbConf genConf sitename = do
  let hosts = (hostList genConf)
  eskrList <- T.sequence $ (\conf -> createSiteMatchKeys conf sitename getAlarmId hosts) <$> dbConf
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
