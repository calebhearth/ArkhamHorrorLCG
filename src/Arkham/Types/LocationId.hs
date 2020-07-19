module Arkham.Types.LocationId where

import           Arkham.Types.Card
import           ClassyPrelude
import           Data.Aeson

newtype LocationId = LocationId { unLocationId :: CardCode }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, IsString)
