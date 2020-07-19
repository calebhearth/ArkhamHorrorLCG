module Arkham.Types.Message
  ( Message(..)
  , Question(..)
  , Source(..)
  , ClueCount(..)
  )
where

import           Arkham.Types.AssetId
import           Arkham.Types.Card
import           Arkham.Types.EnemyId
import           Arkham.Types.InvestigatorId
import           Arkham.Types.LocationId
import           Arkham.Types.Token
import           ClassyPrelude
import           Data.Aeson

newtype ClueCount = ClueCount { unClueCount :: Int }

data Source = AssetSource AssetId
    | InvestigatorSource InvestigatorId
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
    | ChooseActivateCardAbilityAction InvestigatorId
    | ActivateCardAbilityAction InvestigatorId CardCode Int
    | ResolveToken Token InvestigatorId Int Int Message
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
    | EnemyDamage EnemyId Source Int
    | EnemyDefeated EnemyId Source
    | InvestigatorPlayCard InvestigatorId CardCode
    | InvestigatorAssignDamage InvestigatorId EnemyId Int Int
    | AssetDamage AssetId EnemyId Int Int
    | AssetDefeated AssetId
    | AssetDiscarded AssetId CardCode
    | InvestigatorDamage InvestigatorId EnemyId Int Int
    | InvestigatorPlayAsset InvestigatorId AssetId
    | DiscoverClueAtLocation InvestigatorId LocationId
    | DiscoverClue InvestigatorId
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Question = ChooseOne [Question]
    | ChooseOneAtATime [Question]
    | ChoiceResult Message
    | ChooseTo Message
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)
