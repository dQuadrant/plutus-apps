module Marconi.Rpc.Types

where

import Data.Aeson (FromJSON (parseJSON), object, toJSON, (.:), (.:?), (.=))
import Data.Aeson qualified as A
import Data.Aeson.Types (ToJSON)
import Data.Text qualified as T

data RpcRequest = RpcRequest {
    rpcReqVersion :: T.Text,
    rpcReqId      :: RpcId,
    rpcMethod     :: String,
    rpcParams     :: [T.Text]
}

instance FromJSON RpcRequest where
    parseJSON (A.Object o) = RpcRequest
                <$> (o .: "jsonrpc")
                <*> (o.: "id")
                <*> (o.: "method")
                <*> (o .: "params")
    parseJSON  _ = fail "Expected object"


data RpcId = RpcIdNumber Integer
            | RpcIdString T.Text
            | RpcIdNull

instance FromJSON RpcId where
  parseJSON (A.Number  n) =  pure $ RpcIdNumber $ round n
  parseJSON (A.String t)  = pure $ RpcIdString t
  parseJSON A.Null        = pure RpcIdNull
  parseJSON  _            = fail "Not integer or string or null"

instance ToJSON  RpcId where
    toJSON (RpcIdNumber n)   = toJSON n
    toJSON (RpcIdString txt) = toJSON txt
    toJSON RpcIdNull         = A.Null


data RpcError = RpcError {
    rpcErrCode    :: Integer
  , rpcErrMessage :: T.Text
}
instance FromJSON RpcError where
  parseJSON (A.Object obj) =  RpcError <$> obj .: "code" <*> obj .: "message"
  parseJSON _              =  fail "RpcError is not object"

instance ToJSON  RpcError where
  toJSON (RpcError code msg) =  A.object [
    "code" .= code,
    "message" .= msg
    ]

data RpcResponse  = RpcErrorResponse{
        rpcVersion :: T.Text
      , rpcId      :: RpcId
      , rpcError   :: RpcError
    } |
    RpcSuccessResponse {
          rpcVersion :: T.Text
      , rpcId        :: RpcId
      , rpcResult    :: A.Value
    }

instance FromJSON RpcResponse where
  parseJSON (A.Object obj) =  do
    errObj <- obj .:? "error"
    case errObj of
      Just err -> RpcErrorResponse
                  <$> obj .: "jsonrpc"
                  <*> obj .: "id"
                  <*> pure err
      Nothing -> RpcSuccessResponse
                  <$> obj .: "jsonrpc"
                  <*> obj .: "id"
                  <*> obj .: "result"

  parseJSON _              =  fail "RpcError is not object"

instance ToJSON  RpcResponse where
  toJSON (RpcErrorResponse ver _id err) =
    object  [
        "jsonrpc"   .= ver,
        "id"        .= _id,
        "error"     .= err
        ]
  toJSON (RpcSuccessResponse ver _id response) =
    object [
        "jsonrpc"   .= ver,
        "id"        .= _id,
        "result"  .=  response
    ]

mapSuccessResponse ::ToJSON v =>  RpcRequest -> v-> RpcResponse
mapSuccessResponse (RpcRequest version _id _ _ ) resp = RpcSuccessResponse version _id (toJSON resp)

mapErrorResponse :: RpcRequest -> Integer -> T.Text -> RpcResponse
mapErrorResponse (RpcRequest version _id _ _ ) code msg = RpcErrorResponse version _id ( RpcError code msg)
