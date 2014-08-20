{- |
Module      :  <Active.Alarm.Grabber>
Description :  <Function to grab data from database and pair up with the config List>
Copyright   :  (c) <Plow Technology 2014>
License     :  <MIT>

Maintainer  :  <lingpo.huang@plowtech.net>
Stability   :  unstable
Portability :  portable 

<Function to grab data from database and pair up with the config List>
-}

{-# LANGUAGE TypeFamilies, FlexibleContexts, OverloadedStrings #-}
module Active.Alarm.Grabber where

-- General
import Data.List
-- Database
import Database.Persist.Class
import Database.Persist.Types
import Persist.Mongo.Settings
import Database.Persist.MongoDB
-- Control
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Control

createAndMatchKeys :: (MonadIO m, MonadBaseControl IO m, PersistEntity val, Ord a, PersistEntityBackend val ~ MongoBackend) => MongoDBConf -> [Filter val] -> (Entity val -> a) -> [b] -> m [(a, b)]
createAndMatchKeys mongoConf dbfilter collToKey destList = do
  collectionList <- runDBConf mongoConf $ selectList dbfilter []
  let keyList = sort $ collToKey <$> collectionList
  return $ matchKeys keyList destList

matchKeys :: [a] -> [b] -> [(a,b)]
matchKeys aList bList = [ (a, b) | a <- aList, b <- bList ]
 
