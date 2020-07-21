module Arkham.Types.Message
  ( Message(..)
  , Question(..)
  , Source(..)
  , ClueCount(..)
  , PlayerCount(..)
  , EnemyCount(..)
  , CurrentInvestigatorLocation(..)
  )
where

import           Arkham.Types.AssetId
import           Arkham.Types.Card
import           Arkham.Types.EnemyId
import           Arkham.Types.InvestigatorId
import           Arkham.Types.LocationId
import           Arkham.Types.SkillType
import           Arkham.Types.Token
import           Arkham.Types.Trait
import           ClassyPrelude
import           Data.Aeson

newtype EnemyCount = EnemyCount { unEnemyCount :: Int }
newtype ClueCount = ClueCount { unClueCount :: Int }
newtype PlayerCount = PlayerCount { unPlayerCount :: Int }

data CurrentInvestigatorLocation = CurrentInvestigatorLocation

data Source = AssetSource AssetId
    | EnemySource EnemyId
    | InvestigatorSource InvestigatorId
    | TokenSource Token
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
    | ResolveToken Token InvestigatorId Int
    | ChooseMoveAction InvestigatorId
    | ChooseInvestigateAction InvestigatorId
    | Investigate SkillType Int InvestigatorId LocationId
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
    | InvestigatorPlayCard InvestigatorId CardCode Int
    | InvestigatorAssignDamage InvestigatorId EnemyId Int Int
    | AssetDamage AssetId EnemyId Int Int
    | AssetDefeated AssetId
    | AssetDiscarded AssetId CardCode
    | InvestigatorDamage InvestigatorId Source Int Int
    | InvestigatorPlayAsset InvestigatorId AssetId
    | DiscoverClueAtLocation InvestigatorId LocationId
    | DiscoverClue InvestigatorId
    | BeginSkillCheck InvestigatorId SkillType Int Int [Message]
                  [Message]
    | RunSkillCheck Int
    | AddOnFailure Message
    | FailSkillCheck
    | FindAndDrawEncounterCard InvestigatorId
                           (EncounterCardType, [Trait])
    | DrawAnotherToken InvestigatorId Int Token
    | SkillTestEnds
    | ReturnTokens [Token]
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Question = ChooseOne [Question]
    | ChooseOneAtATime [Question]
    | ChoiceResult Message
    | ChooseTo Message
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)
