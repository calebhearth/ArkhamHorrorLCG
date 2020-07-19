module Arkham.Types.Message
  ( Message(..)
  , Question(..)
  , Source(..)
  )
where

import           Arkham.Types.AssetId
import           Arkham.Types.Card
import           Arkham.Types.EnemyId
import           Arkham.Types.InvestigatorId
import           Arkham.Types.LocationId
import           ClassyPrelude
import           Data.Aeson

newtype Source = AssetSource AssetId
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

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
    | EnemyDamaged EnemyId Source Int
    | InvestigatorPlayCard InvestigatorId CardCode
    | InvestigatorAssignDamage InvestigatorId EnemyId Int Int
    | AssetDamage AssetId EnemyId Int Int
    | InvestigatorDamage InvestigatorId EnemyId Int Int
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Question = ChooseOne [Question]
    | ChooseOneAtATime [Question]
    | ChoiceResult Message
    | ChooseTo Message
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)
