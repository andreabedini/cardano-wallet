{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

{- |
Copyright: © 2022 IOHK
License: Apache-2.0

Definition of the decoration of transactions to map tx inputs to the
corrisponding (known) tx outputs
-}

module Cardano.Wallet.DB.Store.Transactions.Decoration
   ( DecoratedTxIns
   , decorateTxInsForRelation
   , lookupTxOutForTxIn
   , lookupTxOutForTxCollateral
   , decorateTxInsForReadTx
   ) where

import Prelude hiding
    ( (.) )

import Cardano.Wallet.DB.Sqlite.Schema
    ( TxCollateral (..), TxIn (..), TxOut (..) )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (..) )
import Cardano.Wallet.DB.Store.Transactions.Model
    ( TxRelation (TxRelation, collateralIns, collateralOuts, ins, outs)
    , TxSet (TxSet)
    , fromTxCollateralOut
    , fromTxOut
    )
import Cardano.Wallet.Read.Eras
    ( EraValue, extractEraValue )
import Cardano.Wallet.Read.Eras.EraFun
    ( applyEraFun )
import Cardano.Wallet.Read.Primitive.Tx.Features.CollateralInputs
    ( getCollateralInputs )
import Cardano.Wallet.Read.Primitive.Tx.Features.Inputs
    ( getInputs )
import Cardano.Wallet.Read.Tx.CollateralInputs
    ( getEraCollateralInputs )
import Cardano.Wallet.Read.Tx.Inputs
    ( getEraInputs )
import Control.Applicative
    ( (<|>) )
import Control.Category
    ( (.) )
import Control.Monad
    ( guard )
import Data.List
    ( find )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( catMaybes )
import Data.Word
    ( Word32 )

import qualified Cardano.Wallet.Primitive.Types.Tx.TxIn as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W
import qualified Cardano.Wallet.Read.Tx as Read
import qualified Data.Map.Strict as Map

type TxOutKey = (TxId, Word32)

toKeyTxIn :: TxIn -> TxOutKey
toKeyTxIn txin = (txInputSourceTxId txin, txInputSourceIndex txin)

toKeyTxCollateral :: TxCollateral -> TxOutKey
toKeyTxCollateral txcol =
    (txCollateralSourceTxId txcol, txCollateralSourceIndex txcol)

-- | A collection of Tx inputs
-- (regular or collateral, refered to by input and order)
-- that are decorated with the values of their corresponding Tx outputs.
newtype DecoratedTxIns = DecoratedTxIns
    { unDecoratedTxIns
        :: Map TxOutKey W.TxOut
    }

instance Semigroup DecoratedTxIns where
    (DecoratedTxIns a) <> (DecoratedTxIns b) = DecoratedTxIns (a <> b)

instance Monoid DecoratedTxIns where
    mempty = DecoratedTxIns mempty

lookupTxOutForTxIn
    :: TxIn -> DecoratedTxIns -> Maybe W.TxOut
lookupTxOutForTxIn tx = Map.lookup (toKeyTxIn tx) . unDecoratedTxIns

lookupTxOutForTxCollateral
    :: TxCollateral -> DecoratedTxIns -> Maybe W.TxOut
lookupTxOutForTxCollateral tx =
    Map.lookup (toKeyTxCollateral tx) . unDecoratedTxIns

decorateTxInsInternal :: TxSet -> [(TxId, Word32)]
    -> [(TxId, Word32)] -> DecoratedTxIns
decorateTxInsInternal (TxSet relations) ins collateralIns =
    DecoratedTxIns . Map.fromList . catMaybes $
        (lookupOutput <$> ins) <> (lookupOutput <$> collateralIns)
  where

    lookupOutput :: (TxId, Word32) -> Maybe ((TxId, Word32), W.TxOut)
    lookupOutput key@(txid, index) = do
        tx <- Map.lookup txid relations
        out <- lookupTxOut tx index <|> lookupTxCollateralOut tx index
        pure (key, out)

    lookupTxOut :: TxRelation -> Word32 -> Maybe W.TxOut
    lookupTxOut tx index = fromTxOut <$>
        find ((index ==) . txOutputIndex . fst) (outs tx)

    lookupTxCollateralOut :: (Enum a, Eq a) => TxRelation -> a -> Maybe W.TxOut
    lookupTxCollateralOut tx index = do
        out <- collateralOuts tx
        let collateralOutputIndex = toEnum $ length (outs tx)
        guard $ index == collateralOutputIndex  -- Babbage leder spec
        pure $ fromTxCollateralOut out

-- | Decorate the Tx inputs of a given 'TxRelation'
-- by searching the 'TxSet' for corresponding output values.
decorateTxInsForRelation
    :: TxSet -> TxRelation -> DecoratedTxIns
decorateTxInsForRelation txSet TxRelation{ins,collateralIns} =
    decorateTxInsInternal txSet (toKeyTxIn <$> ins)
        (toKeyTxCollateral <$>collateralIns)

-- | Decorate the Tx inputs of a given 'TxRelation'
-- by searching the 'TxSet' for corresponding output values.
decorateTxInsForReadTx
    :: TxSet -> EraValue Read.Tx -> DecoratedTxIns
decorateTxInsForReadTx txSet tx
  = decorateTxInsInternal txSet
        (fmap undoWTxIn
            $ extractEraValue $ applyEraFun (getInputs . getEraInputs) tx)
        (fmap undoWTxIn
            $ extractEraValue $ applyEraFun
                (getCollateralInputs . getEraCollateralInputs) tx)
  where
    undoWTxIn :: W.TxIn -> (TxId, Word32)
    undoWTxIn (W.TxIn k n) = (TxId k,n)
