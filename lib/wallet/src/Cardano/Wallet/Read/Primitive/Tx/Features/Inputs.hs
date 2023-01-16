{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Read.Primitive.Tx.Features.Inputs
    ( getInputs
    , mkShelleyTxIns
    )
    where

import Prelude

import Cardano.Wallet.Read.Eras
    ( EraFun (..), K (..) )
import Cardano.Wallet.Read.Primitive.Tx.Shelley
    ( fromShelleyTxIn )
import Cardano.Wallet.Read.Tx.Inputs
    ( Inputs (..), InputsType )
import Data.Foldable
    ( toList )

import qualified Cardano.Ledger.Shelley.API as SH
import qualified Cardano.Wallet.Byron.Compatibility as BYC
import qualified Cardano.Wallet.Primitive.Types.Tx.TxIn as W

getInputs :: EraFun Inputs (K [W.TxIn])
getInputs = EraFun
    { byronFun = \(Inputs ins) -> K . fmap BYC.fromTxIn $ toList ins
    , shelleyFun = mkShelleyTxInputsIns
    , allegraFun = mkShelleyTxInputsIns
    , maryFun = mkShelleyTxInputsIns
    , alonzoFun = mkShelleyTxInputsIns
    , babbageFun = mkShelleyTxInputsIns
    }

mkShelleyTxIns :: Foldable t => (t (SH.TxIn crypto)) -> K [W.TxIn] b
mkShelleyTxIns ins = K . fmap fromShelleyTxIn $ toList ins

mkShelleyTxInputsIns :: (Foldable t, InputsType era ~ t (SH.TxIn crypto))
    => Inputs era -- ^
  -> K [W.TxIn] b
mkShelleyTxInputsIns (Inputs ins) = mkShelleyTxIns ins
