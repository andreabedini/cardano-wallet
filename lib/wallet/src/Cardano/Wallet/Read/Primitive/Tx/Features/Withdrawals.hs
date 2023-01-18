{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Read.Primitive.Tx.Features.Withdrawals
    ( getWithdrawals
    )
    where

import Prelude

import Cardano.Wallet.Read.Eras
    ( EraFun (..), K (..) )
import Cardano.Wallet.Read.Tx.Withdrawals
    ( Withdrawals (..) )
import Data.Map.Strict
    ( Map )

import Cardano.Wallet.Primitive.Types.Coin
    ( Coin )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount )
import Cardano.Wallet.Read.Primitive.Tx.Shelley
    ( fromShelleyWdrl )

getWithdrawals :: EraFun Withdrawals (K (Maybe (Map RewardAccount Coin)))
getWithdrawals = EraFun
    { byronFun = noWithdrawals
    , shelleyFun = yesWithdrawals
    , allegraFun = yesWithdrawals
    , maryFun = yesWithdrawals
    , alonzoFun = yesWithdrawals
    , babbageFun = yesWithdrawals
    }
    where
        noWithdrawals = const $ K Nothing
        yesWithdrawals (Withdrawals ttl)
            = K . Just . fromShelleyWdrl $ ttl

