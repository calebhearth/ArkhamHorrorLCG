module Arkham.Types.AssetId where

import           ClassyPrelude
import           Data.Aeson
import           Data.UUID

newtype AssetId = AssetId { unAssetId :: UUID }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

-- Used to selectively find damageable assets
newtype DamageableAssetId = DamageableAssetId { unDamageableAssetId :: AssetId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)
