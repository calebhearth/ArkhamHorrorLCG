module Arkham.Types.AssetId where

import           ClassyPrelude
import           Data.Aeson
import           Data.UUID

newtype AssetId = AssetId { unAssetId :: UUID }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)
