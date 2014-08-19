{- |
Module      :  <Main.hs>
Description :  <Executable for activealarmgrabber>
Copyright   :  (c) <Plow Technology 2014>
License     :  <MIT>

Maintainer  :  <lingpo.huang@plowtech.net>
Stability   :  unstable
Portability :  portable 

<module description starting at first column>
-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
import           Control.Applicative
import qualified Data.Aeson               as A
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as BSL
import           Data.Maybe
import qualified Data.Text                as T
import qualified Data.Traversable         as T
import           Data.Yaml
import           Database.Persist
import           Database.Persist.Types
import           DirectedKeys.GenerateCfg
import           GHC.Generics
import           Persist.Mongo.Settings
import           Prelude

-- main :: IO ()
-- main = do
--   dbConf <- readDBConf "config.yml"
--   genConf <- readKeyGen "config.yml"
--   let hosts = (hostList genConf)
--   eBoundList <- T.sequence $ (\conf -> createAndMatchKeys conf getAlarmId hosts) <$> dbConf
--   case eBoundList of
--     Left _ -> putStrLn "Error reading config file"
--     Right boundList -> BSL.putStrLn . A.encode $ listToOutput boundList

-- getAlarmId :: Entity Alarm -> Key Alarm
-- getAlarmId = entityKey

-- getOnpingTagPid :: Entity OnpingTagHistory -> Maybe Int
-- getOnpingTagPid = onpingTagHistoryPid . entityVal


-- readKeyGen :: FilePath -> IO KeyGenConfig
-- readKeyGen fp = do
--   contents <- BS.readFile fp
--   case (decodeEither contents) of
--     Left _ -> fail "Unable to parse KeyGenConfig"
--     Right kgcfg -> return kgcfg


-- listToOutput :: [(a,b)] -> [KeyGenOutput b a]
-- listToOutput sList = map (\(host, bound) -> KeyGenOutput bound host ) sList

-- data KeyGenConfig = KeyGenConfig {
--   hostList :: [String]
-- } deriving (Eq, Show, Generic)

-- instance FromJSON KeyGenConfig where

-- data KeyGenOutput a b = KeyGenOutput {
--   keyGenHost  :: a
-- , keyGenBound :: b
-- } deriving (Eq, Show)


-- instance (ToJSON a, ToJSON b) => ToJSON (KeyGenOutput a b) where
--   toJSON (KeyGenOutput {..}) = object ["host" .= keyGenHost
--                                       ,"upperBound" .= keyGenBound]
