{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- |
 Copyright: © 2018-2020 IOHK
 License: Apache-2.0

 This module contains types for address discovery. The two address discovery
 schemes implemented are:

  * "Cardano.Wallet.Primitive.AddressDiscovery.Sequential"
  * "Cardano.Wallet.Primitive.AddressDiscovery.Random"
-}
module Cardano.Wallet.Primitive.AddressDiscovery (
    IsOurs (..),
    IsOwned (..),
    GenChange (..),
    CompareDiscovery (..),
    KnownAddresses (..),
    GetPurpose (..),
    GetAccount (..),
    coinTypeAda,
    MaybeLight (..),
    DiscoverTxs (..),
    PendingIxs,
    emptyPendingIxs,
    pendingIxsToList,
    pendingIxsFromList,
    nextChangeIndex,
    dropLowerPendingIxs,
) where

import Prelude

import Cardano.Crypto.Wallet (
    XPrv,
    XPub,
 )
import Cardano.Wallet.Primitive.AddressDerivation (
    Depth (..),
    DerivationIndex (..),
    DerivationType (..),
    Index (..),
    KeyFingerprint (..),
    RewardAccount,
 )
import Cardano.Wallet.Primitive.BlockSummary (
    ChainEvents,
 )
import Cardano.Wallet.Primitive.Passphrase.Types (
    Passphrase (..),
 )
import Cardano.Wallet.Primitive.Types.Address (
    Address (..),
    AddressState (..),
 )
import Cardano.Wallet.Util (
    invariant,
 )
import Control.DeepSeq (
    NFData,
 )
import Data.Kind (
    Type,
 )
import Data.List.NonEmpty (
    NonEmpty,
 )
import GHC.Generics (
    Generic,
 )

import qualified Cardano.Wallet.Address.Pool as AddressPool
import qualified Data.List as L

{- | Checks whether or not a given entity belongs to us.

 This abstraction exists to give us the ability to keep the wallet business
 logic agnostic to the address derivation and discovery mechanisms.

 This is needed because two different address schemes lives on Cardano:

   - A hierarchical random scheme:
      rather 'custom' made, with several flaws; this is the original and now
      legacy address scheme.

   - A hierarchical sequential scheme:
      a new scheme based on the BIP-0044 specification, which is better suited
      for our present needs.

 In practice, we will need a wallet that can support both, even if not at the
 same time, and this little abstraction can buy us this without introducing
 too much overhead.
-}
class IsOurs s entity where
    isOurs ::
        entity ->
        s ->
        -- | Returns derivation path if the entity is ours, otherwise Nothing.
        (Maybe (NonEmpty DerivationIndex), s)

{- | More powerful than 'isOurs', this abstractions offer the underlying state
 the ability to find / compute the address private key corresponding to a
 given known address.

 Requiring 'IsOwned' as a constraint supposed that there is a way to recover
 the root private key of a particular wallet. This isn't true for externally
 owned wallet which would delegate its key management to a third party (like
 a hardware Ledger or Trezor).
-}
class IsOurs s Address => IsOwned s key ktype where
    isOwned ::
        s ->
        (key 'RootK XPrv, Passphrase "encryption") ->
        Address ->
        -- | Derive the private key corresponding to an address. Careful, this
        -- operation can be costly. Note that the state is discarded from this
        -- function as we do not intend to discover any addresses from this
        -- operation; This is merely a lookup from known addresses.
        Maybe (key ktype XPrv, Passphrase "encryption")

{- | Abstracting over change address generation. In theory, this is only needed
 for sending transactions on a wallet following a particular scheme. This
 abstractions allows for defining an heuristic to pick new change address. For
 instance, in BIP-44, change addresses belong to a particular change chain
 (also called "Internal Chain").
-}
class GenChange s where
    type ArgGenChange s :: Type
    genChange ::
        ArgGenChange s ->
        s ->
        -- | Generate a new change address for the given scheme. The rules for
        -- generating a new change address depends on the underlying scheme.
        (Address, s)

{- | Ordering addresses by discovery date.

 If `a1` has been discovered before `a2`, then the following equation holds:

 @
 compareDiscovery s a1 a2 == LT
 @

 If `a1` has been discovered after `a2`, then the following equation holds:

 @
 compareDiscovery s a1 a2 == GT
 @

 Note that, if an address isn't known it is considered not discovered and
 therefore, is always _greater than_ any known address.
-}
class CompareDiscovery s where
    compareDiscovery ::
        s ->
        Address ->
        Address ->
        Ordering

{- | Extract the list of all known addresses.

 NOTE: Change addresses aren't considered "known" until they've been used. The
 rationale is that, we don't want users or consumers of the wallet to be using
 change addresses prematurely.
-}
class KnownAddresses s where
    knownAddresses ::
        s ->
        [(Address, AddressState, NonEmpty DerivationIndex)]

{- | One master node (seed) can be used for unlimited number of independent
 cryptocoins such as Bitcoin, Litecoin or Namecoin. However, sharing the
 same space for various cryptocoins has some disadvantages.

 This level creates a separate subtree for every cryptocoin, avoiding reusing
 addresses across cryptocoins and improving privacy issues.

 Coin type is a constant, set for each cryptocoin. For Cardano this constant
 is set to 1815' (or 0x80000717). 1815 is the birthyear of our beloved Ada
 Lovelace.

 Hardened derivation is used at this level.
-}
coinTypeAda :: Index 'Hardened 'CoinTypeK
coinTypeAda = toEnum 0x80000717

-- It is used for getting purpose for a given key.
class GetPurpose (key :: Depth -> Type -> Type) where
    getPurpose :: Index 'Hardened 'PurposeK

-- It is used for getting account public key for a given state.
class GetAccount s (key :: Depth -> Type -> Type) | s -> key where
    getAccount :: s -> key 'AccountK XPub

{- | Checks whether the address discovery state @s@ works in light-mode
 and returns a procedure for discovering addresses
 if that is indeed the case.
-}
class MaybeLight s where
    maybeDiscover :: Maybe (LightDiscoverTxs s)

type LightDiscoverTxs s =
    DiscoverTxs (Either Address RewardAccount) ChainEvents s

-- | Function that discovers transactions based on an address.
newtype DiscoverTxs addr txs s = DiscoverTxs
    { discoverTxs ::
        forall m.
        Monad m =>
        (addr -> m txs) ->
        s ->
        m (txs, s)
    }

{-------------------------------------------------------------------------------
                        Pending Tx Change Indexes
-------------------------------------------------------------------------------}

-- | An ordered set of indexes used by pending transactions.
newtype PendingIxs k = PendingIxs
    { pendingIxsToList :: [Index 'Soft k]
    }
    deriving stock (Generic, Show, Eq)

instance NFData (PendingIxs k)

{- | An empty pending set of change indexes.

 NOTE: We do not define a 'Monoid' instance here because there's no rational
 of combining two pending sets.
-}
emptyPendingIxs :: PendingIxs k
emptyPendingIxs = PendingIxs mempty

{- | Construct a 'PendingIxs' from a list, ensuring that it is a set of indexes
 in descending order.
-}
pendingIxsFromList :: [Index 'Soft k] -> PendingIxs k
pendingIxsFromList = PendingIxs . reverse . map head . L.group . L.sort

{- | Get the next change index; If every available indexes have already been
 taken, we'll rotate the pending set and re-use already provided indexes.
-}
nextChangeIndex ::
    forall (key :: Depth -> Type -> Type) k.
    AddressPool.Pool (KeyFingerprint "payment" key) (Index 'Soft k) ->
    PendingIxs k ->
    (Index 'Soft k, PendingIxs k)
nextChangeIndex pool (PendingIxs pendingIndexes) =
    let poolLen = AddressPool.size pool
        gap = AddressPool.gap pool
        firstUnused = toEnum $ poolLen - gap
        lastUnused = toEnum $ poolLen - 1
        (nextIndex, pendingIndexes') =
            case pendingIndexes of
                [] -> (firstUnused, PendingIxs [firstUnused])
                firstIndex : restIndexes ->
                    if length pendingIndexes < AddressPool.gap pool
                        then
                            let next = succ firstIndex
                             in (next, PendingIxs (next : pendingIndexes))
                        else
                            ( firstIndex
                            , PendingIxs (restIndexes <> [firstIndex])
                            )
        errorMessage =
            concat
                [ "Next change index ("
                , show (getIndex nextIndex)
                , ") is NOT between the first unused ("
                , show (getIndex firstUnused)
                , ") and the last unused ("
                , show (getIndex lastUnused)
                , ") indexes. Pool length is "
                , show poolLen
                , ", gap is "
                , show gap
                , ". The pending indexes are: "
                , L.intercalate ", " $ fmap (show . getIndex) pendingIndexes
                ]
     in invariant errorMessage (nextIndex, pendingIndexes') $ \(index, _) ->
            index >= firstUnused && index <= lastUnused

dropLowerPendingIxs :: Index 'Soft k -> PendingIxs k -> PendingIxs k
dropLowerPendingIxs ix (PendingIxs ixs) = PendingIxs $ L.filter (> ix) ixs
