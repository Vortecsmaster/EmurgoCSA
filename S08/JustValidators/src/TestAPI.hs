{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TestAPI where

import Data.Text
import Data.Time (UTCTime)
import Servant.API

-- GET /learners/?sort={name,course}
-- return a listof JSON objects describing users
-- with fields name,course and status

type LearnerAPI = "learner" :> QueryParam "sortby" SortBy :> Get [JSON] [Learner]

data SortBy = Name | Course | Status

data Learner = Learner {
                        name    :: String,
                        course  :: String,
                        status  :: String
                       } deriving Show

type RootEndPoint = Get '[JSON] User

type LearnerAPI2 = "learners" :> "list-all" :> Get '[JSON] [Learner]
              :<|> "list-all" :> "learners" :> Get '[JSON] [Learner]             


