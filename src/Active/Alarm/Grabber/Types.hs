{- |
Module      :  <Active.Alarm.Grabber.Types>
Description :  <Types for Grabber>
Copyright   :  (c) <Plow Technology 2014>
License     :  <MIT>

Maintainer  :  <lingpo.huang@plowtech.net>
Stability   :  unstable
Portability :  portable 

<Types to read from configs and write to YAML file>
-}

{-# LANGUAGE DeriveGeneric, RecordWildCards, OverloadedStrings #-}
module Active.Alarm.Grabber.Types where

import           Control.Applicative
import           Data.Yaml 
import           GHC.Generics (Generic)
import qualified Data.ByteString                as BS

readKeyGen :: FilePath -> IO KeyGenConfig
readKeyGen fp = do
  contents <- BS.readFile fp
  case (decodeEither contents) of
    Left _ -> fail "Unable to parse KeyGenConfig"
    Right kgcfg -> return kgcfg

readSingleKeyRouterConfig :: FilePath -> IO (Either String [KeyGenOutput String String])
readSingleKeyRouterConfig fp = do
  contents <- BS.readFile fp
  return $ decodeEither contents
  

listToOutput :: [(a,b)] -> [KeyGenOutput b a]
listToOutput sList = map (\(des, key) -> KeyGenOutput key des ) sList

data KeyGenConfig = KeyGenConfig {
  hostList :: [String]
} deriving (Eq, Show, Generic)

instance FromJSON KeyGenConfig where

data KeyGenOutput a b = KeyGenOutput {
  keyGenDes  :: a
, keyGenKey :: b
} deriving (Eq, Show, Generic)


instance (ToJSON a, ToJSON b) => ToJSON (KeyGenOutput a b) where
  toJSON (KeyGenOutput {..}) = object ["des" .= keyGenDes
                                      ,"key" .= keyGenKey]

instance (FromJSON a, FromJSON b) => FromJSON (KeyGenOutput a b) where 
    parseJSON  (Object o) = KeyGenOutput <$>  
                            o .: "des" <*>
                            o .: "key"      
    parseJSON _ = fail "Rule, Expecting Object to make KeyGenOutput, recieved Other"

