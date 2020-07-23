module Arkham.Types.Source where

import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import ClassyPrelude
import Data.Aeson

data Source
  = AssetSource AssetId
  | EnemySource EnemyId
  | InvestigatorSource InvestigatorId
  | TokenSource Token
  | AgendaSource AgendaId
  | LocationSource LocationId
  | SkillCheckSource
  | TreacherySource TreacheryId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
