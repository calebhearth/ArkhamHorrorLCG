module Arkham.Types.GameJson where

import           Arkham.Types.Token
import           Arkham.Types.Asset
import           Arkham.Types.AssetId
import           Arkham.Types.Card
import           Arkham.Types.Enemy
import           Arkham.Types.EnemyId
import           Arkham.Types.Investigator
import           Arkham.Types.InvestigatorId
import           Arkham.Types.Location
import           Arkham.Types.LocationId
import           Arkham.Types.Message
import           Arkham.Types.Phase
import           Arkham.Types.Scenario
import           ClassyPrelude
import           Data.Aeson

data GameJson = GameJson
    { gMessages             :: [Message]
    , gScenario             :: Scenario
    , gLocations            :: HashMap LocationId Location
    , gInvestigators        :: HashMap InvestigatorId Investigator
    , gEnemies              :: HashMap EnemyId Enemy
    , gAssets               :: HashMap AssetId Asset
    , gActiveInvestigatorId :: InvestigatorId
    , gLeadInvestigatorId   :: InvestigatorId
    , gPhase                :: Phase
    , gDiscard              :: [CardCode]
    , gTokenBag             :: [Token]
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
