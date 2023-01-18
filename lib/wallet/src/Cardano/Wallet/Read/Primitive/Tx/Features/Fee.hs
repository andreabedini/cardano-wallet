{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Read.Primitive.Tx.Features.Fee
    ( getFee
    )
    where

import Prelude

import Cardano.Ledger.Coin
    ( Coin (unCoin) )
import Cardano.Wallet.Read.Eras
    ( EraFun (..), K (..) )
import Cardano.Wallet.Read.Tx.Fee
    ( Fee (..), FeeType )

import qualified Cardano.Wallet.Primitive.Types.Coin as W

getFee :: EraFun Fee (K (Maybe W.Coin))
getFee = EraFun
    { byronFun = \_ -> K Nothing
    , shelleyFun = mkShelleyTxFee
    , allegraFun = mkShelleyTxFee
    , maryFun = mkShelleyTxFee
    , alonzoFun = mkShelleyTxFee
    , babbageFun = mkShelleyTxFee
    }

mkShelleyTxFee :: (FeeType era ~ Coin)
    => Fee era -- ^
    -> K (Maybe W.Coin) b
mkShelleyTxFee (Fee c) = K $ Just $ W.Coin $ fromIntegral $ unCoin c
