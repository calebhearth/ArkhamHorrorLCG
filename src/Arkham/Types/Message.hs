module Arkham.Types.Message
  ( Message(..)
  , Question(..)
  )
where

import           Arkham.Types.Card
import           Arkham.Types.EnemyId
import           Arkham.Types.InvestigatorId
import           Arkham.Types.LocationId
import           ClassyPrelude
import           Data.Aeson

data Message = Setup
    | BeginInvestigation
    | PlaceLocation LocationId
    | RevealLocation LocationId
    | MoveAllTo LocationId
    | MoveTo InvestigatorId LocationId
    | PlayerWindow InvestigatorId
    | Ask Question
    | ChooseTakeResourceAction InvestigatorId
    | ChooseDrawCardAction InvestigatorId
    | ChoosePlayCardAction InvestigatorId
    | ChooseActiveCardAbilityAction InvestigatorId
    | ChooseMoveAction InvestigatorId
    | ChooseInvestigateAction InvestigatorId
    | ChooseFightEnemyAction InvestigatorId
    | ChooseEvadeEnemyAction InvestigatorId
    | ChooseEngageEnemyAction InvestigatorId
    | ChooseEndTurn InvestigatorId
    | CheckAttackOfOpportunity InvestigatorId
    | TakeResources InvestigatorId Int
    | EnemyWillAttack InvestigatorId EnemyId
    | EnemyAttacks [Message]
    | EnemyAttack InvestigatorId EnemyId
    | InvestigatorDrawEncounterCard InvestigatorId CardCode
    | InvestigatorDrawEnemy InvestigatorId LocationId EnemyId
    | EnemySpawn LocationId EnemyId
    | EnemyEngageInvestigator EnemyId InvestigatorId
    | InvestigatorPlayCard InvestigatorId CardCode
    | InvestigatorAssignDamage InvestigatorId EnemyId Int Int
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Question = ChooseOne [Question]
    | ChooseOneAtATime [Question]
    | ChoiceResult Message
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)
