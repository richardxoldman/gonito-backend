{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


module Handler.Poleval where


import           Import                     hiding (Proxy, encodeUtf8, fromList, defaultManagerSettings, newManager, httpLbs)

import           Control.Lens               hiding ((.=), (^.))
import           Data.HashMap.Strict.InsOrd (fromList)
import           Data.Proxy                 as DPR
import           Data.Swagger               hiding (delete, get, tags)
import qualified Data.Swagger               as DS
import           Data.Swagger.Declare

--import GHC.Generics
import Data.Aeson
--import Data.Aeson.Types (parseMaybe, (.:))
import Network.HTTP.Client
import Data.ByteString.UTF8 (toString)


data AdditionalRequest = AdditionalRequest
    { challenge :: String
    , dev_expected :: String
    , dev_out :: String
    , testA_expected :: String
    , testA_out :: String
    , testB_expected :: String
    , testB_out :: String
    , dev_in :: String
    , testA_in :: String
    , testB_in :: String
    } deriving (Generic, Show, Read)

instance ToJSON AdditionalRequest where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON AdditionalRequest


getDataFromPythonEndpoint :: IO ()
getDataFromPythonEndpoint = do
    dataExpected <- readFile "expected.tsv"
    dataIn <- readFile "in.tsv"
    dataOut <- readFile "out.tsv"

    let exemplaryRequest = AdditionalRequest
            { challenge = "QuestionAnswering"
            , dev_expected = toString dataExpected
            , dev_out = toString dataOut
            , testA_expected = ""
            , testA_out = ""
            , testB_expected = ""
            , testB_out = ""
            , dev_in = toString dataIn
            , testA_in = ""
            , testB_in = ""
            }

    send $ RequestBodyLBS $ encode exemplaryRequest


buildRequest :: String -> RequestBody -> IO Request
buildRequest givenUrl body = do
    initRequest <- parseRequest givenUrl
    return $ initRequest { method = "GET", requestBody = body }


send :: RequestBody -> IO ()
send toSend = do
    manager <- newManager defaultManagerSettings
    request <- buildRequest "http://127.0.0.1:8000" toSend
    response <- httpLbs request manager

    let Just obj = decode (responseBody response)

    print (obj :: Object)


postPolevalR :: Handler Html
postPolevalR = do
    pure "abc"


polevalApi :: Swagger
polevalApi = spec & definitions .~ defs
    where
        (defs, spec) = runDeclare declarePolevalApi mempty


declarePolevalApi :: Declare (Definitions Schema) Swagger
declarePolevalApi = do
    response <- declareResponse (DPR.Proxy :: DPR.Proxy ())

    pure $ mempty & paths .~ fromList
        [
            ("/api/poleval", mempty
            & DS.post ?~ (mempty
                & parameters .~ []
                & produces ?~ MimeList ["application/json"]
                & description ?~ "Calculate metrics specyfic for poleval."
                & at 200 ?~ Inline response
                )
            )
        ]

