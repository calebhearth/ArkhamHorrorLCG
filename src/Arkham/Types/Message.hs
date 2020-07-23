module Arkham.Types.Message
  ( Message(..)
  , Question(..)
  , Source(..)
  , ClueCount(..)
  , PlayerCount(..)
  , EnemyCount(..)
  , InvestigatorLocation(..)
  , LeadInvestigatorId(..)
  , AllInvestigators(..)
  , Prey(..)
  )
where

import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.SkillType
import Arkham.Types.Token
import Arkham.Types.Trait
import Arkham.Types.TreacheryId
import ClassyPrelude
import Data.Aeson

newtype EnemyCount = EnemyCount { unEnemyCount :: Int }
newtype ClueCount = ClueCount { unClueCount :: Int }

instance Semigroup ClueCount where
  (ClueCount a) <> (ClueCount b) = ClueCount (a + b)

instance Monoid ClueCount where
  mempty = ClueCount 0
  mappend = (<>)

newtype PlayerCount = PlayerCount { unPlayerCount :: Int }
newtype LeadInvestigatorId = LeadInvestigatorId { unLeadInvestigatorId :: InvestigatorId }

newtype InvestigatorLocation = InvestigatorLocation InvestigatorId
data AllInvestigators = AllInvestigators
data Prey = AnyPrey | HighestSkill SkillType | LowestSkill SkillType
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Source
  = AssetSource AssetId
  | EnemySource EnemyId
  | InvestigatorSource InvestigatorId
  | TokenSource Token
  | AgendaSource AgendaId
  | LocationSource LocationId
  | SkillCheckSource
  | TreacherySource TreacheryId
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Message
  = Setup
  | BeginRound
  | EndRoundWindow
  | EndRound
  | BeginMythos
  | EndMythos
  | BeginInvestigation
  | EndInvestigation
  | BeginEnemy
  | EndEnemy
  | BeginUpkeep
  | EndUpkeep
  | HuntersMove
  | EnemiesAttack
  | ReadyExhausted
  | AllDrawCardAndResource
  | AllCheckHandSize
  | PlaceDoomOnAgenda
  | AdvanceAgendaIfThresholdSatisfied
  | AdvanceAgenda AgendaId
  | AdvanceAct ActId
  | AllDrawEncounterCard
  | PlaceLocation LocationId
  | PlacedLocation LocationId
  | AddConnection LocationId LocationSymbol
  | AddConnectionBack LocationId LocationSymbol
  | AddedConnection LocationId LocationId
  | RevealLocation LocationId
  | RemoveLocation LocationId
  | RemoveEnemy EnemyId
  | MoveAllTo LocationId
  | MoveTo InvestigatorId LocationId
  | PrePlayerWindow
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
  | InvestigatorDrawEncounterCard InvestigatorId
  | InvestigatorDrawEnemy InvestigatorId LocationId EnemyId
  | EnemySpawn LocationId EnemyId
  | EnemyEngageInvestigator EnemyId InvestigatorId
  | EnemyDamage EnemyId Source Int
  | EnemyDefeated EnemyId Source
  | InvestigatorPlayCard InvestigatorId CardCode Int
  | InvestigatorAssignDamage InvestigatorId EnemyId Int Int
  | AssetDamage AssetId EnemyId Int Int
  | AssetDefeated AssetId
  | DiscardAsset AssetId
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
  | DrawToken Token
  | EmptyDeck InvestigatorId
  | DrawCards InvestigatorId Int
  | PayCardCost InvestigatorId CardCode Int
  | AddAct ActId
  | AddAgenda AgendaId
  | AllRandomDiscard
  | NextAgenda AgendaId AgendaId
  | NextAct ActId ActId
  | WhenEnterLocation InvestigatorId LocationId
  | AfterEnterLocation InvestigatorId LocationId
  | EnemyMove EnemyId LocationId LocationId
  | CreateEnemyAt CardCode LocationId
  | RunTreachery InvestigatorId TreacheryId
  | RevelationSkillCheck InvestigatorId SkillType Int [Message] [Message]
  | DamagePerPointOfFailure InvestigatorId
  | HorrorPerPointOfFailure InvestigatorId
  | DiscardTreachery TreacheryId
  | SetEncounterDeck [EncounterCard]
  | TreacheryFailure InvestigatorId TreacheryId -- TODO: better name
  | ChooseAndDiscardAsset InvestigatorId
  | AttackEnemy InvestigatorId EnemyId SkillType Int Int
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Question
  = ChooseOne [Question]
  | ChooseOneAtATime [Question]
  | ChoiceResult Message
  | ChoiceResults [Message]
  | ChooseTo Message
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
