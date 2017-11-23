module Cardano.Wallet.API.V1.Settings where

import           Cardano.Wallet.API.V1.Types

import           Servant

type API =
         "settings" :> Summary "Retrieves the static settings for this node."
                    :> Get '[JSON] WalletSettings
