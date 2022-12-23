-- |
-- This module bootstraps the mamba JSON RPC server, it acts as a glue conntecting the
-- JSON-RPC, HttpServer, marconiIndexer, and marconi cache
--
module Marconi.Bootstrap  where

import Cardano.Api (ChainPoint (ChainPointAtGenesis), NetworkId)
import Cardano.BM.Setup (withTrace)
import Cardano.BM.Trace (logError)
import Cardano.BM.Tracing (defaultConfigStdout)
import Cardano.Streaming (ChainSyncEventException (NoIntersectionFound), withChainSyncEventStream)
import Control.Exception (catch)
import Control.Lens ((^.))
import Marconi.Api.HttpServer qualified as Http
import Marconi.Api.Types (CliArgs (CliArgs), HasDBQueryEnv (queryTMVar), HasJsonRpcEnv (queryEnv),
                          JsonRpcEnv (JsonRpcEnv, _HttpSettings, _QueryEnv), RpcPortNumber, TargetAddresses)
import Marconi.Api.UtxoIndexersQuery qualified as QIUtxo
import Marconi.Indexers (combineIndexers, queryAwareUtxoWorker)
import Marconi.Logging (logging)
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Prettyprinter (defaultLayoutOptions, layoutPretty, pretty, (<+>))
import Prettyprinter.Render.Text (renderStrict)


-- | Bootstraps the JSON-RPC  http server with appropriate settings and marconi cache
-- this is just a wrapper for the bootstrapHttp in json-rpc package
bootstrapJsonRpc
    :: FilePath
    -> Maybe RpcPortNumber
    -> TargetAddresses
    -> NetworkId
    -> IO JsonRpcEnv
bootstrapJsonRpc dbPath maybePort targetAddresses nId = do

    queryenv <- QIUtxo.bootstrap dbPath targetAddresses nId
    let httpsettings =  maybe defaultSettings (flip setPort defaultSettings ) (case maybePort of
          Nothing   ->  Just 3345
          Just port -> Just port )
    pure $ JsonRpcEnv
        { _HttpSettings = httpsettings
        , _QueryEnv = queryenv
        }

bootstrapHttp
    :: JsonRpcEnv
    -> IO ()
bootstrapHttp  = Http.bootstrap

-- |  marconi cardano blockchain indexer
bootstrapUtxoIndexers
    :: CliArgs
    -> JsonRpcEnv
    -> IO ()
bootstrapUtxoIndexers (CliArgs socket dbPath _ networkId targetAddresses) env =
    do
        let qsem = env ^. queryEnv . queryTMVar
            indexers = combineIndexers [( queryAwareUtxoWorker qsem targetAddresses , dbPath)]
            chainPoint = ChainPointAtGenesis
        c <- defaultConfigStdout
        withTrace c "marconi-mamba" $ \trace ->
          withChainSyncEventStream socket networkId chainPoint (indexers . logging trace)
          `catch` \NoIntersectionFound ->
            logError trace $
                renderStrict $
                    layoutPretty defaultLayoutOptions $
                        "No intersection found when looking for the chain point"
                        <+> (pretty . show  $ chainPoint)  <> "."
                        <+> "Please check the slot number and the block hash do belong to the chain"


-- | Exit program with error
-- Note, if the targetAddress parser fails, or is empty, there is nothing to do for the hotStore.
-- In such case we should fail fast
fromJustWithError :: (Show e) => Either e a -> a
fromJustWithError v = case v of
    Left e ->
        error $ "\n!!!\n Abnormal Termination with Error: " <> show e <> "\n!!!\n"
    Right accounts -> accounts
