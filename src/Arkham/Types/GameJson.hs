module Arkham.Types.GameJson where

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
    , gActiveInvestigatorId :: InvestigatorId
    , gPhase                :: Phase
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
