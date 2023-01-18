{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Read.Primitive.Tx.Features.CollateralOutputs
    ( getCollateralOutputs
    )
    where

import Prelude

import Cardano.Wallet.Read.Eras
    ( EraFun (..), K (..) )
import Cardano.Wallet.Read.Primitive.Tx.Babbage
    ( fromBabbageTxOut )
import Cardano.Wallet.Read.Tx.CollateralOutputs
    ( CollateralOutputs (..) )
import Data.Maybe.Strict
    ( strictMaybeToMaybe )

import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W

getCollateralOutputs :: EraFun CollateralOutputs (K (Maybe W.TxOut))
getCollateralOutputs = EraFun
    { byronFun = \_ -> K Nothing
    , shelleyFun = \_ -> K Nothing
    , allegraFun = \_ -> K Nothing
    , maryFun = \_ -> K Nothing
    , alonzoFun = \_ -> K Nothing
    , babbageFun = \(CollateralOutputs mo)
        -> K $ fromBabbageTxOut <$> strictMaybeToMaybe mo
    }
