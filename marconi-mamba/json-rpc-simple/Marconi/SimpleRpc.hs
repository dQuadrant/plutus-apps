{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Marconi.SimpleRpc where
import Servant ( type (:>), ReqBody, Post, JSON, Server )
import Marconi.Rpc.Types (RpcRequest (RpcRequest), RpcResponse, mapSuccessResponse, mapErrorResponse)
import Control.Monad.IO.Class ()


type SimpleRpcAPI =
        ReqBody '[JSON] RpcRequest :> Post '[JSON] RpcResponse


simpleRpcServer :: Server SimpleRpcAPI
simpleRpcServer x = pure $ simpleRpcHandler x

simpleRpcHandler :: RpcRequest -> RpcResponse
simpleRpcHandler req@(RpcRequest _ _ method params ) = case method of
        "echo"  -> mapSuccessResponse req params
        _any    -> mapErrorResponse req 332 "Unknown method"
