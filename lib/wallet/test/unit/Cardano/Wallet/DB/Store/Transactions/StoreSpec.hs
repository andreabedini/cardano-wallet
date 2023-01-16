{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.DB.Store.Transactions.StoreSpec
    ( spec
    ) where

import Prelude

import Cardano.DB.Sqlite
    ( ForeignKeysSetting (..) )
import Cardano.Wallet.DB.Arbitrary
    ()
import Cardano.Wallet.DB.Fixtures
    ( StoreProperty, assertWith, logScale, withDBInMemory, withStoreProp )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (TxId) )
import Cardano.Wallet.DB.Store.Transactions.Model
    ( DeltaTxSet (..), TxSet (..), mkTxSet )
import Cardano.Wallet.DB.Store.Transactions.Store
    ( mkStoreTransactions, selectTx )
import Cardano.Wallet.Primitive.Types.Tx
    ( Tx (..) )
import Control.Monad
    ( forM_ )
import Data.DBVar
    ( Store (..) )
import Data.Delta
    ( Delta (..) )
import Test.DBVar
    ( GenDelta, prop_StoreUpdates )
import Test.Hspec
    ( Spec, around, describe, it )
import Test.QuickCheck
    ( Gen, arbitrary, elements, frequency, property )
import Test.QuickCheck.Monadic
    ( forAllM )

import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
    around (withDBInMemory ForeignKeysEnabled) $ do
        describe "Transactions store" $ do
            it "respects store laws" $
                property . prop_StoreLaws
        describe "selectTx" $
            it "retrieves transaction that was written" $
                property . prop_selectTx

{-----------------------------------------------------------------------------
    Properties
------------------------------------------------------------------------------}

prop_StoreLaws :: StoreProperty
prop_StoreLaws = withStoreProp $ \runQ ->
    prop_StoreUpdates
        runQ
        mkStoreTransactions
        (pure mempty)
        (logScale . genDeltas)

addCBOR :: Tx -> Gen Tx
addCBOR tx = do
    mcbor <- arbitrary
    pure $ tx{txCBOR = mcbor}

-- | Generate interesting changes to 'TxSet'.
genDeltas :: GenDelta DeltaTxSet
genDeltas (TxSet pile) =
    frequency
        [ (8, Append . mkTxSet <$> (arbitrary >>= mapM addCBOR))
        , (1, DeleteTx . TxId <$> arbitrary)
        ,
            ( 2
            , DeleteTx
                <$> if null pile
                    then TxId <$> arbitrary
                    else elements (Map.keys pile)
            )
        ]

genTxSet :: Gen TxSet
genTxSet = (`apply` mempty) <$> genDeltas mempty

prop_selectTx :: StoreProperty
prop_selectTx =
    withStoreProp $ \runQ ->
        forAllM genTxSet $ \txs -> do
            runQ $ writeS mkStoreTransactions txs
            forM_ (Map.assocs $ relations txs) $ \(txid, tx) -> do
                Just tx' <- runQ $ selectTx txid
                assertWith "relation is consistent" $ tx == tx'
