module Arkham.Types.InvestigatorId where

import           Arkham.Types.Card
import           ClassyPrelude
import           Data.Aeson

newtype InvestigatorId = InvestigatorId { unInvestigatorId :: CardCode }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, IsString)
