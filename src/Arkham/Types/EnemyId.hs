module Arkham.Types.EnemyId where

import           ClassyPrelude
import           Data.Aeson
import           Data.UUID

newtype EnemyId = EnemyId { unEnemyId :: UUID }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)
