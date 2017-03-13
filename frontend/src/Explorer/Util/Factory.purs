module Explorer.Util.Factory where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Time.NominalDiffTime (mkTime)
import Pos.Core.Types (Coin(..))
import Pos.Explorer.Web.ClientTypes (CAddress(..), CAddressSummary(..), CHash(..), CTxEntry(..), CTxId(..))


mkCHash :: String -> CHash
mkCHash = CHash

mkCTxId :: String -> CTxId
mkCTxId =
    CTxId <<< mkCHash

mkCoin :: Int -> Coin
mkCoin coin =
  Coin {getCoin: coin}

mkCAddress :: String -> CAddress
mkCAddress = CAddress

-- All the following helper function `mkEmpty**` are for debugging only
-- We do need these to mock live data
-- It can be removed if all endpoints are ready

mkEmptyCTxEntry :: CTxEntry
mkEmptyCTxEntry = CTxEntry
    { cteId: mkCTxId "--"
    , cteTimeIssued: Just $ mkTime 0.0
    , cteAmount: mkCoin 0
    }

mkEmptyCAddressSummary :: CAddressSummary
mkEmptyCAddressSummary = CAddressSummary
    { caAddress: mkCAddress "--"
    , caTxNum: 0
    , caBalance: mkCoin 0
    , caTxList: []
    }
--
-- mkEmptyCTxDetailed :: CTxDetailed
-- mkEmptyCTxDetailed = CTxDetailed
--     { ctdId: mkCTxId "-"
--     , ctdTimeIssued: mkTime 0.0
--     , ctdType: mkEmptyCTxType
--     }

-- mkEmptyCTxType :: CTxType
-- mkEmptyCTxType =
--     CTxIncoming [mkCAddress "--"] $ mkCoin 0
