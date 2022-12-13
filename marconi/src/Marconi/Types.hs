{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE TypeFamilies       #-}

-- | This module provides several type aliases and utility functions to deal with them.
module Marconi.Types
       (
       -- * Addresses alias used to query marconi
       CardanoAddress,
       TargetAddresses(..),
       AddressList,
       -- * Aliases for the current Cardano era
       CurrentEra,
       pattern AsCurrentEra,
       pattern CurrentEra,
       TxOut,
       -- * Aliases to ease concept mapping between plutus types and cardano types
       TxOutRef,
       txOutRef
       ) where

import Cardano.Api qualified as C

import Data.List.NonEmpty (NonEmpty)


type CardanoAddress = C.Address C.ShelleyAddr

type AddressList = NonEmpty CardanoAddress
-- | Typre represents non empty list of Bech32 compatable addresses
data TargetAddresses =      TargetAllAddresses
              |             NoTargetAddresses
              |             TargetAddresses !AddressList
              deriving (Show)

instance Semigroup TargetAddresses where
       (<>)   TargetAllAddresses   _                  =      TargetAllAddresses
       (<>)   _               TargetAllAddresses      =      TargetAllAddresses
       (<>)   (TargetAddresses l1) (TargetAddresses l2) =      TargetAddresses (l1 <> l2)
       (<>)   v1@(TargetAddresses _) _                  =      v1
       (<>)   _                      v2                 =      v2

instance Monoid TargetAddresses where
       mempty = NoTargetAddresses

-- | An alias for the current era, to ease the transition from one era to the next one
type CurrentEra = C.BabbageEra

pattern CurrentEra :: C.CardanoEra CurrentEra
pattern CurrentEra = C.BabbageEra

pattern AsCurrentEra :: C.AsType CurrentEra
pattern AsCurrentEra = C.AsBabbageEra

-- | A Cardano TxOut of the current Era
type TxOut = C.TxOut C.CtxTx CurrentEra

-- | A reference to a transaction output. This is a
-- pair of a transaction reference, and an index indicating which of the outputs
-- of that transaction we are referring to.
type TxOutRef = C.TxIn

txOutRef :: C.TxId -> C.TxIx -> C.TxIn
txOutRef = C.TxIn
