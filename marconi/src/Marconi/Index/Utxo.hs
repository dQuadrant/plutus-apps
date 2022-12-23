{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Marconi.Index.Utxo
  ( -- * UtxoIndex
    UtxoIndex,
    Depth (..),
    open,
    Ix.insert,
    Ix.rewind,
    UtxoUpdate (..),
    UtxoRow (..),
    toRows,
    inputs,
    outputs,
    slotNo,
    address,
    TxOut,
  )
where

import Cardano.Api
  ( Hash,
    ScriptData,
    ScriptDataJsonSchema (ScriptDataJsonDetailedSchema),
    ScriptInAnyLang,
    SerialiseAddress (serialiseAddress),
    SlotNo,
    TxIn (TxIn),
    TxOutDatum (TxOutDatumHash, TxOutDatumInline),
    TxOutValue (TxOutAdaOnly, TxOutValue),
    Value,
    lovelaceToValue,
    serialiseToRawBytesHexText,
  )
import Cardano.Api qualified as C
import Cardano.Api.Shelley (ReferenceScript (ReferenceScript, ReferenceScriptNone))
import Control.Exception (SomeException (SomeException), throwIO, try)
import Control.Lens.Operators ((&), (^.))
import Control.Lens.TH (makeLenses)
import Control.Monad (when)
import Data.Aeson (ToJSON (toJSON), (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy (toStrict)
import Data.Foldable (forM_, toList)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Database.SQLite.Simple (Error (ErrorConstraint), Only (Only), SQLData (SQLBlob, SQLInteger, SQLText), SQLError (SQLError))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.FromField (FromField (fromField), ResultError (ConversionFailed), returnError)
import Database.SQLite.Simple.FromRow (FromRow (fromRow), field)
import Database.SQLite.Simple.ToField (ToField (toField))
import Database.SQLite.Simple.ToRow (ToRow (toRow))
import GHC.Generics (Generic)
import Marconi.Types (CurrentEra, TxOut, TxOutRef, txOutRef)
import RewindableIndex.Index.VSqlite (SqliteIndex)
import RewindableIndex.Index.VSqlite qualified as Ix
import System.Random.MWC (createSystemRandom, uniformR)

data UtxoUpdate = UtxoUpdate
  { _inputs :: !(Set TxIn),
    _outputs :: ![(TxOut, TxOutRef)],
    _slotNo :: !SlotNo
  }
  deriving (Show)

$(makeLenses ''UtxoUpdate)

type Result = Maybe [TxOutRef]

type UtxoIndex = SqliteIndex UtxoUpdate () C.AddressAny Result

newtype Depth = Depth Int

instance FromField C.AddressAny where
  fromField f =
    fromField f
      >>= maybe
        (returnError ConversionFailed f "Cannot deserialise address.")
        pure
        . C.deserialiseAddress C.AsAddressAny

instance ToField C.AddressAny where
  toField = SQLText . serialiseAddress

instance FromField C.TxId where
  fromField f = do
    f' <- fromField f
    let val = Text.encodeUtf8 f'
    case C.deserialiseFromRawBytesHex (C.proxyToAsType Proxy) val of
      Left _ -> returnError ConversionFailed f "Cannot deserialise TxId."
      Right txId' -> pure txId'

instance ToField C.TxId where
  toField = SQLText . serialiseToRawBytesHexText

instance FromField C.TxIx where
  fromField = fmap C.TxIx . fromField

instance ToField C.TxIx where
  toField (C.TxIx i) = SQLInteger $ fromIntegral i

data UtxoRow = UtxoRow
  { _address :: !C.AddressAny,
    _outRef :: !TxOutRef,
    _datHash :: !(Maybe (Hash ScriptData)),
    _inlineDatum :: !(Maybe ScriptData),
    _val :: !Value,
    _refScript :: !(Maybe ScriptInAnyLang)
  }
  deriving (Generic, Show)

instance Eq UtxoRow where
  (UtxoRow a1 t1 _ _ _ _) == (UtxoRow a2 t2 _ _ _ _) = a1 == a2 && t1 == t2

instance Ord UtxoRow where
  compare (UtxoRow a1 t1 _ _ _ _) (UtxoRow a2 t2 _ _ _ _) = case compare a1 a2 of
    EQ -> compare t1 t2
    x -> x

instance ToJSON UtxoRow where
  toJSON (UtxoRow _ outRef datHash inlineDat val refScript) =
    Aeson.object
      [ "txIn" .= outRef,
        "value" .= val,
        "datum"
          .= ( case (inlineDat, datHash) of
                 (Nothing, Nothing) -> Aeson.Null
                 (Nothing, Just datHash') -> toJSON datHash'
                 (Just sd, _) -> C.scriptDataToJson ScriptDataJsonDetailedSchema sd
             ),
        "referenceScript" .= refScript
      ]

$(makeLenses ''UtxoRow)

instance FromRow UtxoRow where
  fromRow = UtxoRow <$> field <*> (txOutRef <$> field <*> field) <*> field <*> field <*> field <*> field

instance ToRow UtxoRow where
  toRow u =
    [ toField $ u ^. address,
      head $ toRow $ u ^. outRef,
      (toRow $ u ^. outRef) !! 1,
      toField $ u ^. datHash,
      toField $ u ^. inlineDatum,
      toField $ u ^. val,
      toField $ u ^. refScript
    ]

instance FromRow TxOutRef where
  fromRow = txOutRef <$> field <*> field

instance ToRow TxOutRef where
  toRow (TxIn txOutRefId txOutRefIdx) =
    [ toField txOutRefId,
      toField txOutRefIdx
    ]

instance FromField (Hash ScriptData) where
  fromField f =
    fromField f
      >>= maybe
        (returnError ConversionFailed f "Cannot deserialise datum hash.")
        pure
        . C.deserialiseFromRawBytes (C.AsHash C.AsScriptData)

instance ToField (Hash ScriptData) where
  toField hs = SQLBlob $ C.serialiseToRawBytes hs

instance FromField ScriptData where
  fromField f = do
    val' <-
      fromField f
        >>= maybe
          (returnError ConversionFailed f "Cannot deserialise datum hash.")
          pure
          . Aeson.decode
    case C.scriptDataFromJson ScriptDataJsonDetailedSchema val' of
      Left _ -> returnError ConversionFailed f "Cannot deserialise inline datum."
      Right dt -> pure dt

instance ToField ScriptData where
  toField sd = SQLBlob $ toStrict $ Aeson.encode $ C.scriptDataToJson ScriptDataJsonDetailedSchema sd

instance FromField Value where
  fromField f = do
    bs <- fromField f
    case Aeson.decode bs :: Maybe Value of
      Nothing -> returnError ConversionFailed f "Cannot deserialise datum hash."
      Just val' -> pure val'

instance ToField Value where
  toField v = SQLBlob $ toStrict $ Aeson.encode v

instance FromField ScriptInAnyLang where
  fromField f = do
    bs <- fromField f
    case Aeson.decode bs :: Maybe ScriptInAnyLang of
      Nothing -> returnError ConversionFailed f "Cannot deserialise datum hash."
      Just val' -> pure val'

instance ToField ScriptInAnyLang where
  toField v = SQLBlob $ toStrict $ Aeson.encode v

open ::
  FilePath ->
  Depth ->
  IO UtxoIndex
open dbPath (Depth k) = do
  -- The second parameter ((k + 1) * 2) specifies the amount of events that are buffered.
  -- The larger the number, the more RAM the indexer uses. However, we get improved SQL
  -- queries due to batching more events together.
  ix <- fromJust <$> Ix.newBoxed query store onInsert k ((k + 1) * 2) dbPath
  let c = ix ^. Ix.handle
  SQL.execute_ c "CREATE TABLE IF NOT EXISTS utxos (address TEXT NOT NULL, txId TEXT NOT NULL, inputIx INT NOT NULL, datumHash TEXT, inlineDatum TEXT, value TEXT, refScript TEXT, PRIMARY KEY (txId,inputIx))"
  SQL.execute_ c "CREATE TABLE IF NOT EXISTS spent (txId TEXT NOT NULL, inputIx INT NOT NULL, PRIMARY KEY (txId,inputIx))"
  pure ix

query ::
  UtxoIndex ->
  C.AddressAny ->
  [UtxoUpdate] ->
  IO Result
query ix addr updates = do
  -- SELECT all utxos that have not been spent.
  let c = ix ^. Ix.handle
  -- Create indexes initially. When created this should be a no-op.
  SQL.execute_ c "CREATE INDEX IF NOT EXISTS utxo_address ON utxos (address)"
  SQL.execute_ c "CREATE UNIQUE INDEX IF NOT EXISTS utxo_refs ON utxos (txId, inputIx)"
  SQL.execute_ c "CREATE UNIQUE INDEX IF NOT EXISTS spent_refs ON spent (txId, inputIx)"

  -- Perform the db query
  storedUtxos <- SQL.query c "SELECT address, txId, inputIx FROM utxos LEFT JOIN spent ON utxos.txId = spent.txId AND utxos.inputIx = spent.inputIx WHERE utxos.txId IS NULL AND utxos.address = ?" (Only addr)
  let memoryUtxos = concatMap (filter (onlyAt addr) . toRows) updates
      spentOutputs = foldMap _inputs updates
  buffered <- Ix.getBuffer $ ix ^. Ix.storage
  let bufferedUtxos = concatMap (filter (onlyAt addr) . toRows) buffered
  pure . Just $
    storedUtxos ++ bufferedUtxos ++ memoryUtxos
      -- Remove utxos that have been spent (from memory db).
      & filter (\u -> not (_outRef u `Set.member` spentOutputs))
      & map _outRef

store :: UtxoIndex -> IO ()
store ix = do
  buffer <- Ix.getBuffer $ ix ^. Ix.storage
  let utxos = concatMap toRows buffer
      spent = concatMap (toList . _inputs) buffer
      c = ix ^. Ix.handle
      
  -- Try inserting if UNIQUE constraint fails then ignore as that utxo is already there otherwise continue
  result <- try (performInsert c utxos spent) :: IO (Either SQLError ())
  case result of
    Left (SQLError ErrorConstraint _ _) -> do
      SQL.execute_ c "ROLLBACK"
    Left err -> throwIO err
    Right _ -> pure ()

  -- We want to perform vacuum about once every 100 * buffer ((k + 1) * 2)
  rndCheck <- createSystemRandom >>= uniformR (1 :: Int, 100)
  when (rndCheck == 42) $ do
    SQL.execute_ c "DELETE FROM utxos WHERE utxos.rowid IN (SELECT utxos.rowid FROM utxos LEFT JOIN spent on utxos.txId = spent.txId AND utxos.inputIx = spent.inputIx WHERE spent.txId IS NOT NULL)"
    SQL.execute_ c "VACUUM"
  where
    performInsert :: SQL.Connection -> [UtxoRow] -> [TxIn] -> IO ()
    performInsert c utxos spent = do
      SQL.execute_ c "BEGIN"
      forM_ utxos $
        SQL.execute c "INSERT INTO utxos (address, txId, inputIx, datumHash, inlineDatum, value, refScript) VALUES (?, ?, ?, ?, ?, ?, ?)"
      forM_ spent $
        SQL.execute c "INSERT INTO spent (txId, inputIx) VALUES (?, ?)"
      SQL.execute_ c "COMMIT"

onInsert :: UtxoIndex -> UtxoUpdate -> IO [()]
onInsert _ix _update = pure []

toRows :: UtxoUpdate -> [UtxoRow]
toRows update =
  update ^. outputs
    & map
      ( \(C.TxOut addr val' datum refScript', outRef') ->
          UtxoRow
            { _address = toAddr addr,
              _outRef = outRef',
              _datHash = case datum of
                TxOutDatumHash _ dh -> Just dh
                _ -> Nothing,
              _inlineDatum = case datum of
                TxOutDatumInline _ inDat -> Just inDat
                _ -> Nothing,
              _val = case val' of
                TxOutAdaOnly _ lov -> lovelaceToValue lov
                TxOutValue _ val'' -> val'',
              _refScript = case refScript' of
                ReferenceScript _ sial -> Just sial
                ReferenceScriptNone -> Nothing
            }
      )
  where
    toAddr :: C.AddressInEra CurrentEra -> C.AddressAny
    toAddr (C.AddressInEra C.ByronAddressInAnyEra addr) = C.AddressByron addr
    toAddr (C.AddressInEra (C.ShelleyAddressInEra _) addr) = C.AddressShelley addr

onlyAt :: C.AddressAny -> UtxoRow -> Bool
onlyAt address' row = address' == row ^. address
