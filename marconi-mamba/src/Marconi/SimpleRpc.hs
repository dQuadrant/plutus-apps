{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Marconi.SimpleRpc where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)
import Marconi.Api.Types (DBQueryEnv (DBQueryEnv))
import Marconi.Api.UtxoIndexersQuery qualified as Q.Utxo
import Marconi.Rpc.Types (RpcRequest (RpcRequest), RpcResponse, mapErrorResponse, mapSuccessResponse)
import Servant (Handler, JSON, Post, ReqBody, Server, type (:>))


type SimpleRpcAPI =
        ReqBody '[JSON] RpcRequest :> Post '[JSON] RpcResponse


simpleRpcServer :: DBQueryEnv -> Server SimpleRpcAPI
simpleRpcServer = simpleRpcHandler

simpleRpcHandler :: DBQueryEnv -> RpcRequest -> Handler RpcResponse
simpleRpcHandler env req@(RpcRequest _ _ method params ) = case method of
        "echo"         -> pure $ mapSuccessResponse req params
        "ada_getutxos" -> liftIO $ getUtxosFromIndexer env req $ head params
        _any           -> pure $ mapErrorResponse req 332 "Unknown method"


getUtxosFromIndexer :: DBQueryEnv -> RpcRequest -> Text -> IO RpcResponse
getUtxosFromIndexer env req addr = do
        utxosE <- Q.Utxo.findByAddress env addr
        case utxosE of
          Left _      -> pure $ mapErrorResponse req 500 "Query utxo failed."
          Right utxos -> pure $ mapSuccessResponse req utxos
