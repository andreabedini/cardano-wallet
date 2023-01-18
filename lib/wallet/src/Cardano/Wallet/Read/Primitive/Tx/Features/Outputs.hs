{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Read.Primitive.Tx.Features.Outputs
    ( getOutputs
    )
    where

import Prelude

import Cardano.Wallet.Read.Eras
    ( EraFun (..), K (..) )
import Cardano.Wallet.Read.Primitive.Tx.Alonzo
    ( fromAlonzoTxOut )
import Cardano.Wallet.Read.Primitive.Tx.Babbage
    ( fromBabbageTxOut )
import Cardano.Wallet.Read.Primitive.Tx.Mary
    ( fromMaryTxOut )
import Cardano.Wallet.Read.Primitive.Tx.Shelley
    ( fromShelleyTxOut )
import Cardano.Wallet.Read.Tx.Outputs
    ( Outputs (..) )
import Data.Foldable
    ( toList )

import qualified Cardano.Wallet.Byron.Compatibility as BYC
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W

getOutputs :: EraFun Outputs (K [W.TxOut])
getOutputs = EraFun
    { byronFun = \(Outputs ins) -> K . fmap BYC.fromTxOut $ toList ins
    , shelleyFun = \(Outputs os) -> K . fmap fromShelleyTxOut $ toList os
    , allegraFun = \(Outputs os) -> K . fmap fromShelleyTxOut $ toList os
    , maryFun = \(Outputs os) -> K . fmap fromMaryTxOut $ toList os
    , alonzoFun = \(Outputs os) -> K . fmap fromAlonzoTxOut $ toList os
    , babbageFun = \(Outputs os) -> K . fmap fromBabbageTxOut $ toList os
    }
