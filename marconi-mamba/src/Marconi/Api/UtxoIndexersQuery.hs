module Marconi.Api.UtxoIndexersQuery
    ( bootstrap
    , findByCardanoAddress
    , findByAddress
    , findAll
    , reportQueryAddresses
    , UtxoRow(..)
    , reportQueryCardanoAddresses
    , reportBech32Addresses
    , withQueryAction
    ) where
import Cardano.Api qualified as CApi
import Control.Concurrent.Async (concurrently, forConcurrently)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVar, putTMVar, takeTMVar)
import Control.Exception (bracket)
import Control.Lens ((^.))
import Control.Monad.STM (STM)
import Data.Functor ((<&>))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set, fromList, toList, union)
import Data.Text (Text, intercalate, unpack)
import Database.SQLite.Simple (NamedParam ((:=)), execute_, open)
import Database.SQLite.Simple qualified as SQL
import Marconi.Api.Types (DBConfig (DBConfig, utxoConn),
                          DBQueryEnv (DBQueryEnv, _DbConf, _Network, _QueryAddresses, _QueryTMVar),
                          HasDBQueryEnv (dbConf, queryAddresses, queryTMVar),
                          QueryExceptions (AddressNotInListError, QueryError), TargetAddresses,
                          UtxoQueryTMVar (UtxoQueryTMVar), UtxoTxOutReport (UtxoTxOutReport), unUtxoIndex)
import Marconi.Index.Utxo (UtxoIndex, UtxoRow (UtxoRow), toRows)
import Marconi.Types (CardanoAddress, TargetAddresses (..), TxOutRef)
import RewindableIndex.Index.VSqlite qualified as Ix
import Cardano.Api (prettyPrintJSON)
import qualified Data.ByteString.Char8 as BS8
import System.IO (openFile, IOMode (WriteMode))
import qualified Data.ByteString as BS
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified RewindableIndex.Index.VSplit as Ix

-- | Bootstraps the utxo query environment.
-- The module is responsible for accessing SQLite for quries.
-- The main issue we try to avoid here is mixing inserts and quries in SQLite to avoid locking the database
bootstrap
    ::  FilePath                -- ^ file path the SQLite utxos database
    -> TargetAddresses          -- ^ user provided target addresses
    -> CApi.NetworkId           -- ^ cardano networkId
    -> IO DBQueryEnv            -- ^ returns Query runtime environment
bootstrap dbPath targetAddresses nId = do
    connection <- open dbPath
    execute_ connection "PRAGMA journal_mode=WAL"
    ix <- atomically (newEmptyTMVar :: STM ( TMVar UtxoIndex) )
    pure $ DBQueryEnv
        { _DbConf = DBConfig connection
        , _QueryTMVar = UtxoQueryTMVar ix
        , _QueryAddresses = targetAddresses
        , _Network = nId
        }
-- | finds reports for all user-provided addresses.
-- TODO we need to use streaming
--
findAll
    :: DBQueryEnv                   -- ^ Query run time environment
    -> IO (Set UtxoTxOutReport)     -- ^ set of corresponding TxOutRefs
findAll env = fromList <$> forConcurrently addresses f
    where
        addresses = case env ^. queryAddresses of
          TargetAllAddresses -> [] -- targetting by all address for query is not supported
          NoTargetAddresses  -> []
          TargetAddresses ne -> NonEmpty.toList ne

        f  :: CardanoAddress -> IO UtxoTxOutReport
        f addr = UtxoTxOutReport (CApi.serialiseAddress addr) <$> findByCardanoAddress env (CApi.toAddressAny addr)

-- | Query utxos address address
--
utxoQuery:: DBConfig -> CApi.AddressAny -> IO (Set UtxoRow)
utxoQuery dbConfig address = do
    utxoList <- SQL.queryNamed (utxoConn dbConfig)
                  "SELECT address, utxos.txid, utxos.inputIx, datumHash, inlineDatum, value, refScript FROM utxos LEFT JOIN spent ON utxos.txId = spent.txId AND utxos.inputIx = spent.inputIx WHERE spent.txId IS NULL AND utxos.address=:address"
                  [":address" := address]
    pure $ fromList utxoList

-- | Query utxos by Cardano Address
--  To Cardano error may occure
findByCardanoAddress
    :: DBQueryEnv           -- ^ Query run time environment
    -> CApi.AddressAny      -- ^ Cardano address to query
    -> IO (Set UtxoRow)
findByCardanoAddress env address = do
    
    withQueryAction env  address utxoQuery

-- | Retrieve a Set of TxOutRefs associated with the given Cardano Era address
-- We return an empty Set if no address is found
findByAddress
    :: DBQueryEnv                                   -- ^ Query run time environment
    -> Text                                         -- ^ Bech32 Address
    -> IO (Either QueryExceptions UtxoTxOutReport)   -- ^ To Plutus address conversion error may occure
findByAddress env addressText =
    let
        f :: Either CApi.Bech32DecodeError CardanoAddress -> IO (Either QueryExceptions UtxoTxOutReport)
        f (Right address)
            |isIndexed address  =
                -- allow for targetAddress search only
              ((pure . CApi.toAddressAny $ address)
              >>= findByCardanoAddress env)
              <&>  (Right . UtxoTxOutReport addressText)
            | otherwise = pure . Left . AddressNotInListError . QueryError $
              unpack addressText <> " not in the provided target addresses"
        f (Left e) = pure . Left $ QueryError (unpack  addressText
                     <> " generated error: "
                     <> show e)
        isIndexed addr = (case env ^. queryAddresses of
                        TargetAllAddresses -> True
                        NoTargetAddresses  -> False
                        TargetAddresses ne -> addr `elem` ne )
    in
        f $ CApi.deserialiseFromBech32 CApi.AsShelleyAddress addressText

-- | query in-momory utxos for the given address
--
queryInMemory
    :: CApi.AddressAny          -- ^ address to query
    -> UtxoIndex                -- ^ inmemory, hot-store, storage for utxos
    -> IO ( Set UtxoRow )
queryInMemory address ix = do
    let
        isTargetAddress :: UtxoRow -> Bool
        isTargetAddress (UtxoRow a _ _ _ _ _) =  address == a

    memUtxos <- Ix.getFullBuffer (ix ^. Ix.storage)
    BSL.writeFile "/tmp/mem.json" (A.encode memUtxos)
    let res= (fromList
                . filter isTargetAddress
                . concatMap toRows) memUtxos
    putStrLn $ "MemUtxos: " ++ show (length memUtxos) ++ "\nUserMemUtxos:" ++ (BS8.unpack $ prettyPrintJSON res)
    pure res
-- | Execute the query function
-- We must stop the utxo inserts before doing the query
withQueryAction
    :: DBQueryEnv                                           -- ^ Query run time environment
    -> CApi.AddressAny                                      -- ^ Cardano address to query
    -> (DBConfig -> CApi.AddressAny -> IO (Set UtxoRow) )  -- ^ Query function to run
    -> IO (Set UtxoRow)
withQueryAction env address qAction =
    let
        utxoIndexer = unUtxoIndex  $ env ^. queryTMVar
        action ndxr =  do
            (fromColdStore, fromHotStore) <- concurrently
                (qAction (env ^. dbConf) address)
                (queryInMemory address ndxr )
            pure . union fromColdStore $ fromHotStore
    in
        bracket
          (atomically $ takeTMVar  utxoIndexer)
          (atomically . (putTMVar utxoIndexer))
          action

-- | report target addresses
-- Used by JSON-RPC
reportQueryAddresses
    :: DBQueryEnv
    -> IO (Set CardanoAddress)
reportQueryAddresses env
    = pure
    . fromList
    $ (case env ^. queryAddresses of
                TargetAllAddresses -> []
                NoTargetAddresses  -> []
                TargetAddresses ne -> NonEmpty.toList  ne)

reportQueryCardanoAddresses
    :: DBQueryEnv
    -> Text
reportQueryCardanoAddresses  = intercalate ", " . toList . reportBech32Addresses

reportBech32Addresses
    :: DBQueryEnv
    -> Set Text
reportBech32Addresses env
    = fromList
    . fmap CApi.serialiseAddress
    $ (case env ^. queryAddresses of
            TargetAllAddresses -> []
            NoTargetAddresses  -> []
            TargetAddresses ne -> NonEmpty.toList ne)
