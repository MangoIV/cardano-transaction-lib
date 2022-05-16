{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Ogmios.Query (
  makeRequest,
  tryQueryUntilZero,
  queryCurrentProtocolParameters,
  decodeProtocolParameters,
) where

--------------------------------------------------------------------------------

import Control.Exception (IOException, SomeException (SomeException))
import Control.Exception.Base (Exception, catch, try)
import Data.Typeable (tyConModule, tyConPackage, typeOf, typeRepTyCon)

import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson qualified as Aeson

import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict qualified as HashMap
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Debug.Trace (trace)
import Network.Socket (withSocketsDo)
import Network.WebSockets (ClientApp, ConnectionException)
import Network.WebSockets qualified as WebSockets

import Cardano.Api (FromJSON)
import Cardano.Api.Shelley (ProtocolParameters)
import Cardano.Api.Shelley qualified as Shelley

--------------------------------------------------------------------------------

data QueryOption = CurrentProtocolParameters

data RequestOption
  = Query QueryOption
  | -- | TODO : add point parameter to Acquire
    Acquire

data ServerParameters = ServerParameters
  { getHost :: String
  , -- | WebSockets needs the port as Int.
    getPort :: Int
  , getPath :: String
  }

newtype ProtocolParametersWrapper = ProtocolParametersWrapper {unwrapParams :: Shelley.ProtocolParameters}

instance FromJSON ProtocolParametersWrapper where
  parseJSON =
    Aeson.withObject "ProtocolParametersWrapper" $ \top -> do
      o :: Aeson.Object <- top .: "result"
      v <- o .: "protocolVersion"
      params <-
        Shelley.ProtocolParameters
          <$> ((,) <$> v .: "major" <*> v .: "minor")
          <*> o .: "decentralizationParameter"
          <*> o .: "extraEntropy"
          <*> o .: "maxBlockHeaderSize"
          <*> o .: "maxBlockBodySize"
          <*> o .: "maxTxSize"
          <*> o .: "minFeeConstant" -- I think minFeeConstant and minFeeCoefficient are swapped here
          -- but this is consistent with the current config file.
          <*> o .: "minFeeCoefficient"
          <*> o .: "minUTxOValue" -- Don't know the corresponding value
          <*> o .: "stakeKeyDeposit"
          <*> o .: "poolDeposit"
          <*> o .: "minPoolCost"
          <*> o .: "poolRetirementEpochBound"
          <*> o .: "desiredNumberOfPools"
          <*> o .: "poolInfluence"
          <*> o .: "monetaryExpansion"
          <*> o .: "treasuryExpansion"
          <*> o .:? "coinsPerUTxOWord"
          <*> o .:? "costModels" .!= Map.empty
          <*> o .:? "prices"
          <*> o .:? "maxExecutionUnitsPerTransaction"
          <*> o .:? "maxExecutionUnitsPerBlock"
          <*> o .:? "maxValueSize"
          <*> o .:? "collateralPercentage"
          <*> o .:? "maxCollateralInputs"
      return $ ProtocolParametersWrapper params

-- Current Ogmios JSON has structure :
-- {
--  "result" : {
--    "costModels" : {
--      "plutus:v1" :
--    }
--  }
-- }
--
-- But the already defined instance of fromJSON of cardano expect
-- {
--  "result" : {
--    "costModels" : {
--      "PlutusScriptV1" :
--    }
--  }
-- }
-- So, modifyPlutusName changes it to the right name.
modifyPlutusName :: Aeson.Value -> Maybe Aeson.Value
modifyPlutusName (Aeson.Object keyHashMap) =
  do
    result <- HashMap.lookup "result" keyHashMap >>= unwrappObject
    costModels <- HashMap.lookup "costModels" result >>= unwrappObject
    plutus <- HashMap.lookup "plutus:v1" costModels
    let newCostModel =
          HashMap.insert "PlutusScriptV1" plutus (HashMap.delete "plutus:v1" costModels)
        newResult =
          HashMap.insert "costModels" (Aeson.Object newCostModel) result
        newKeyHashMap =
          HashMap.insert "result" (Aeson.Object newResult) keyHashMap
    return $ Aeson.Object newKeyHashMap
  where
    unwrappObject :: Aeson.Value -> Maybe Aeson.Object
    unwrappObject (Aeson.Object obj) = Just obj
    unwrappObject _ = Nothing
modifyPlutusName _ = Nothing

decodeProtocolParameters :: ByteString -> Maybe Shelley.ProtocolParameters
decodeProtocolParameters response =
  let value :: Maybe Aeson.Value
      value = Aeson.decode response >>= modifyPlutusName
      wrapped :: Maybe ProtocolParametersWrapper
      wrapped =
        value
          >>= ( Aeson.decode
                  @ProtocolParametersWrapper
                  . Aeson.encode
              )
   in unwrapParams <$> wrapped

defaultServerParameters :: ServerParameters
defaultServerParameters =
  ServerParameters
    { getHost = "localhost"
    , getPort = 1337
    , getPath = "/"
    }

getQueryName :: QueryOption -> Text
getQueryName CurrentProtocolParameters = "currentProtocolParameters"

buildRequestJSON :: RequestOption -> Text
buildRequestJSON (Query CurrentProtocolParameters) =
  "{ \"type\": \"jsonwsp/request\", \"version\": \"1.0\", \"servicename\": \"ogmios\","
    <> "\"methodname\": \"Query\", \"args\": { \"query\":\""
    <> getQueryName CurrentProtocolParameters
    <> "\"} }"
buildRequestJSON _ = error "Unsuported Ogmios request"

mkApp :: RequestOption -> ClientApp ByteString
mkApp option conn = do
  let requestJSON = buildRequestJSON option
  WebSockets.sendTextData
    conn
    requestJSON
  msg <- WebSockets.receiveData conn
  WebSockets.sendClose conn ("" :: Text)
  return msg

makeRequest :: ServerParameters -> RequestOption -> IO ByteString
makeRequest ServerParameters {..} option =
  withSocketsDo $
    WebSockets.runClient getHost getPort getPath $
      mkApp option

queryCurrentProtocolParameters :: IO ByteString
queryCurrentProtocolParameters =
  makeRequest defaultServerParameters $ Query CurrentProtocolParameters

tryQueryUntilZero :: IO ByteString -> Int -> IO (Either String ByteString)
tryQueryUntilZero query remainAttempts =
  if remainAttempts <= 0
    then return $ Left "Error trying to connect to Ogmios"
    else do
      msgOrError <- (try query :: IO (Either IOException ByteString))
      case msgOrError of
        Right msg -> return $ Right msg
        _ -> tryQueryUntilZero query (remainAttempts - 1)

test :: IO ByteString -> IO ()
test query = do
  eitherMsg <- (try query :: IO (Either IOException ByteString))
  case eitherMsg of
    Right msg ->
      ( case (Aeson.decode msg :: Maybe Aeson.Value) of
          Just (Aeson.Object hashmap) -> do
            print hashmap
            putStrLn ""
            print msg
          _ -> print (Text.unpack "Error, wrong format")
      )
    _ -> print (Text.unpack "Error connecting to Ogmios")