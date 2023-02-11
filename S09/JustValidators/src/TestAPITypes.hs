{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}

module TestAPITypes where

import Data.Text
import Data.Time (UTCTime)
import Servant.API

-- GET /learners/?sort={name,course}
-- return a listof JSON objects describing users
-- with fields name,course and status

type LearnerAPI = "learner" :> QueryParam "sortby" SortBy :> Get '[JSON] [Learner]

data SortBy = Name | Course | Status

data Learner = Learner {
                        name    :: String,
                        course  :: String,
                        status  :: String
                       } deriving Show

type RootEndPoint = Get '[JSON] Learner

type LearnerAPI2 = "learners" :> "list-all" :> Get '[JSON] [Learner]
              :<|> "list-all" :> "learners" :> Get '[JSON] [Learner]             

type LearnerAPI3 = "learners" :> "list-all" :> "now" :> Get '[JSON] [Learner]

-- data Verb method (statusCode :: Nat) (contentType :: [*]) a
-- type Get = Verb 'GET 200

-- type Delete = Verb 'DELETE 200
-- type Patch  = Verb 'PATCH 200
-- type Post   = Verb 'POST 200
-- type Put    = Verb 'PUT 200

type PostCreated = Verb 'POST 201
type PostAccepted = Verb 'POST 202

type LearnerAPI4 = "learners" :> Get '[JSON] [Learner]
            :<|> "admins":> Get '[JSON] [Learner]

-- data Stream (method :: k1) (status :: Nat) (framing :: *) (contentType :: *) (a :: *)

type StreamGet  = Stream 'GET 200
type StreamPost = Stream 'POST 200

-- data Capture (s :: Symbol) a
-- s :: Symbol just says that 's' must be a type-level string.

type LearnerAPI5 = "learner" :> Capture "learnerid" Integer :> Get '[JSON] [Learner]
              :<|> "learner" :> Capture "learnerid" Integer :> DeleteNoContent

              -- equivalent to 'GET /learner/:learnerid'
              -- except that we explicitly say that "learnerid"
              -- must be an integer
              -- equivalent to 'DELETE /learner/:learnerid'
-- data QueryParam (sym :: Symbol) a
-- data QueryParams (sym :: Symbol) a
-- data QueryFlag (sym :: Symbol)

type LearnerAPI6 = "learners" :> QueryParam "sortby" SortBy :> Get '[JSON] [Learner]
                -- equivalent to 'GET /learners?sortby={name,}'

-- data ReqBody (contentTypes :: [*]) a

type LearnerAPI7 = "learners" :> ReqBody '[JSON] Learner :> Post '[JSON] Learner
                -- - equivalent to 'POST /learners' with a JSON object
                --   describing a Learner in the request body
                -- - returns a Learner encoded in JSON

           :<|> "learners" :> Capture "learnerid" Integer
                        :> ReqBody '[JSON] Learner
                        :> Put '[JSON] Learner
                -- - equivalent to 'PUT /users/:userid' with a JSON
                --   object describing a User in the request body
                -- - returns a User encoded in JSON

--data Header (sym :: Symbol) a

type LearnerAPI8 = "learners" :> Header "Learner-Agent" Text :> Get '[JSON] [Learner]

type LearnerAPI9 = "learners" :> Get '[JSON, PlainText, FormUrlEncoded, OctetStream] [Learner]

-- data Headers (ls :: [*]) a

type LearnerAPI10 = "learners" :> Get '[JSON] (Headers '[Header "Learner-Count" Integer] [Learner])

-- data BasicAuth (realm :: Symbol) (userData :: *)

-- type ProtectedAPI11 
--    = LearnerAPI
--    :<|> BasicAuth "my-realm" Learner :> LearnerAPI2

type LearnerAPI12 innerAPI
    = LearnerAPI
    :<|> "inner" :> innerAPI

type LearnerAPI12Alone = LearnerAPI12 EmptyAPI

type LearnerAPI13 = "learners" :> Get '[JSON] [Learner]
                 -- a /learners endpoint
            :<|> Raw
                 -- requests to anything else than /learners
                 -- go here, where the server will try to
                 -- find a file with the right name
                 -- at the right path

type LearnerAPI14 = Raw
               :<|> "learners" :> Get '[JSON] [Learner]
                 -- In this situation, the /learners endpoint
                 -- will not be reachable because the Raw
                 -- endpoint matches requests before

type LearnerAPI15 = "files" :> Raw
                 -- The raw endpoint is under the /files
                 -- static path, so it won't match /learners.
            :<|> "learners" :> Get '[JSON] [Learner]

type LearnerAPI16 = "learners" :> Get '[JSON] [Learner]
            :<|> Raw
                 -- The Raw endpoint is matched last, so
                 -- it won't overlap another endpoint.