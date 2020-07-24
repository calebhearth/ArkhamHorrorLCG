{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator
  ( isPrey
  , hasEndedTurn
  , remainingHealth
  , lookupInvestigator
  , GetInvestigatorId(..)
  , Investigator
  )
where

import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Cards.AgnesBaker
import Arkham.Types.Investigator.Cards.AkachiOnyele
import Arkham.Types.Investigator.Cards.AmandaSharpe
import Arkham.Types.Investigator.Cards.AshcanPete
import Arkham.Types.Investigator.Cards.CalvinWright
import Arkham.Types.Investigator.Cards.CarolynFern
import Arkham.Types.Investigator.Cards.DaisyWalker
import Arkham.Types.Investigator.Cards.DexterDrake
import Arkham.Types.Investigator.Cards.DianaStanley
import Arkham.Types.Investigator.Cards.FatherMateo
import Arkham.Types.Investigator.Cards.FinnEdwards
import Arkham.Types.Investigator.Cards.HarveyWalters
import Arkham.Types.Investigator.Cards.JacquelineFine
import Arkham.Types.Investigator.Cards.JennyBarnes
import Arkham.Types.Investigator.Cards.JimCulver
import Arkham.Types.Investigator.Cards.JoeDiamond
import Arkham.Types.Investigator.Cards.LeoAnderson
import Arkham.Types.Investigator.Cards.LolaHayes
import Arkham.Types.Investigator.Cards.LukeRobinson
import Arkham.Types.Investigator.Cards.MandyThompson
import Arkham.Types.Investigator.Cards.MarieLambeau
import Arkham.Types.Investigator.Cards.MarkHarrigan
import Arkham.Types.Investigator.Cards.MinhThiPhan
import Arkham.Types.Investigator.Cards.NathanielCho
import Arkham.Types.Investigator.Cards.NormanWithers
import Arkham.Types.Investigator.Cards.PatriceHathaway
import Arkham.Types.Investigator.Cards.PrestonFairmont
import Arkham.Types.Investigator.Cards.RexMurphy
import Arkham.Types.Investigator.Cards.RitaYoung
import Arkham.Types.Investigator.Cards.RolandBanks
import Arkham.Types.Investigator.Cards.SefinaRousseau
import Arkham.Types.Investigator.Cards.SilasMarsh
import Arkham.Types.Investigator.Cards.SisterMary
import Arkham.Types.Investigator.Cards.SkidsOToole
import Arkham.Types.Investigator.Cards.StellaClark
import Arkham.Types.Investigator.Cards.TommyMuldoon
import Arkham.Types.Investigator.Cards.TonyMorgan
import Arkham.Types.Investigator.Cards.UrsulaDowns
import Arkham.Types.Investigator.Cards.WendyAdams
import Arkham.Types.Investigator.Cards.WilliamYorick
import Arkham.Types.Investigator.Cards.WinifredHabbamock
import Arkham.Types.Investigator.Cards.ZoeySamaras
import Arkham.Types.Investigator.Runner
import Arkham.Types.InvestigatorId
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Stats
import ClassyPrelude
import Data.Aeson
import Data.Coerce
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Lens.Micro.Extras
import Safe (fromJustNote)

allInvestigators :: HashMap InvestigatorId Investigator
allInvestigators = HashMap.fromList $ map
  (\s -> (investigatorId . investigatorAttrs $ s, s))
  [ RolandBanks rolandBanks
  , DaisyWalker daisyWalker
  , SkidsOToole skidsOToole
  , AgnesBaker agnesBaker
  ]

investigatorAttrs :: Investigator -> Attrs
investigatorAttrs = \case
  RolandBanks attrs -> coerce attrs
  DaisyWalker attrs -> coerce attrs
  SkidsOToole attrs -> coerce attrs
  AgnesBaker attrs -> coerce attrs

data Investigator
  = RolandBanks RolandBanksI
  | DaisyWalker DaisyWalkerI
  | SkidsOToole SkidsOTooleI
  | AgnesBaker AgnesBakerI
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance (InvestigatorRunner env) => RunMessage env Investigator where
  runMessage msg = \case
    RolandBanks x -> RolandBanks <$> runMessage msg x
    DaisyWalker x -> DaisyWalker <$> runMessage msg x
    SkidsOToole x -> SkidsOToole <$> runMessage msg x
    AgnesBaker x -> AgnesBaker <$> runMessage msg x

lookupInvestigator :: InvestigatorId -> Investigator
lookupInvestigator iid =
  fromJustNote ("Unkown investigator: " <> show iid)
    $ HashMap.lookup iid allInvestigators

instance HasCardCode Investigator where
  getCardCode = getCardCode . investigatorAttrs

instance HasInvestigatorStats Stats () Investigator where
  getStats _ i = Stats
    { health = investigatorHealth - investigatorHealthDamage
    , sanity = investigatorSanity - investigatorSanityDamage
    , willpower = skillValueFor SkillWillpower a
    , intellect = skillValueFor SkillIntellect a
    , combat = skillValueFor SkillCombat a
    , agility = skillValueFor SkillAgility a
    }
    where a@Attrs {..} = investigatorAttrs i

instance HasSet AssetId () Investigator where
  getSet _ = investigatorAssets . investigatorAttrs

instance HasLocation Investigator where
  locationOf = investigatorLocation . investigatorAttrs

instance HasClueCount Investigator where
  getClueCount = ClueCount . investigatorClues . investigatorAttrs

instance HasSkill Investigator where
  getSkill skillType = skillValueFor skillType . investigatorAttrs

class GetInvestigatorId a where
  getInvestigatorId :: a -> InvestigatorId

instance GetInvestigatorId Investigator where
  getInvestigatorId = investigatorId . investigatorAttrs

isPrey
  :: (HasSet Int SkillType env, HasSet RemainingHealth () env)
  => Prey
  -> env
  -> Investigator
  -> Bool
isPrey AnyPrey _ _ = True
isPrey (HighestSkill skillType) env i =
  fromMaybe 0 (maximumMay . HashSet.toList $ getSet skillType env)
    == skillValueFor skillType (investigatorAttrs i)
isPrey LowestHealth env i =
  fromMaybe
      100
      (minimumMay . map unRemainingHealth . HashSet.toList $ getSet () env)
    == remainingHealth i

hasEndedTurn :: Investigator -> Bool
hasEndedTurn = view endedTurn . investigatorAttrs

remainingHealth :: Investigator -> Int
remainingHealth i = investigatorHealth attrs - investigatorHealthDamage attrs
  where attrs = investigatorAttrs i
