{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Marconi.SimpleRpc where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)
import Marconi.Api.Types (DBQueryEnv (DBQueryEnv), QueryExceptions, UtxoTxOutReport (..))
import Marconi.Api.UtxoIndexersQuery qualified as Q.Utxo
import Marconi.Rpc.Types (RpcRequest (RpcRequest), RpcResponse, mapErrorResponse, mapSuccessResponse)
import Servant (Handler, JSON, Post, ReqBody, Server, type (:>))
import qualified Data.Text as T
import Data.Char (toLower)
import Cardano.Api (ToJSON)
import Data.Functor ((<&>))
import Control.Lens ((^.))
import Marconi.Index.Utxo (UtxoRow(UtxoRow))
import qualified Data.Set as S


type SimpleRpcAPI =
        ReqBody '[JSON] RpcRequest :> Post '[JSON] RpcResponse


simpleRpcServer :: DBQueryEnv -> Server SimpleRpcAPI
simpleRpcServer = simpleRpcHandler

simpleRpcHandler :: DBQueryEnv -> RpcRequest -> Handler RpcResponse
simpleRpcHandler env req@(RpcRequest _ _ method params ) = case map toLower  method of
        "echo"         -> pure $ mapSuccessResponse req params
        "ada_getutxos" -> liftIO $ 
                                Q.Utxo.findByAddress env (head params)  <&> returnQueryResult req
        "ada_getbalance" -> liftIO $ 
                                Q.Utxo.findByAddress env (head params)   <&> fmap sumValue <&> returnQueryResult req
        _any           -> pure $ mapErrorResponse req 332 "Unknown method"



sumValue (UtxoTxOutReport _ utxoRow) = foldMap  rowVal  ( S.toList utxoRow)
        where
        rowVal (UtxoRow _ _ _ _ val _ ) = val


returnQueryResult :: (ToJSON v, Show ex) =>   RpcRequest -> Either ex v -> RpcResponse
returnQueryResult req v = do
        case v of
          Left ex      ->  mapErrorResponse req 500 (T.pack $ "Query utxo failed :" ++ show ex)
          Right result -> mapSuccessResponse req result
