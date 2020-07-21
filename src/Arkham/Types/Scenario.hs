{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Scenario
  ( lookupScenario, Scenario )
where

import           Arkham.Types.Card
import           Arkham.Types.Classes
import           Arkham.Types.Difficulty
import           Arkham.Types.Message
import           Arkham.Types.ScenarioId
import qualified Arkham.Types.Token      as Token
import           Arkham.Types.Trait
import           ClassyPrelude
import           Data.Aeson
-- import           Data.Coerce
import qualified Data.HashMap.Strict     as HashMap
import           Safe                    (fromJustNote)

lookupScenario :: ScenarioId -> Difficulty -> Scenario
lookupScenario = fromJustNote "Unknown scenario" . flip HashMap.lookup allScenarios

allScenarios :: HashMap ScenarioId (Difficulty -> Scenario)
allScenarios = HashMap.fromList
  [ ("01104", theGathering)
  , ("01120", theMidnightMasks)
  ]

data Attrs = Attrs
    { scenarioName       :: Text
    , scenarioId         :: ScenarioId
    , scenarioDifficulty :: Difficulty
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Scenario = TheGathering TheGatheringI
    | TheMidnightMasks TheMidnightMasksI
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- scenarioAttrs :: Scenario -> Attrs
-- scenarioAttrs = \case
--   TheGathering attrs -> coerce attrs
--   TheMidnightMasks attrs -> coerce attrs

baseAttrs :: CardCode -> Text -> Difficulty -> Attrs
baseAttrs cardCode name difficulty = Attrs
  { scenarioId = ScenarioId cardCode
  , scenarioName = name
  , scenarioDifficulty = difficulty
  }

newtype TheGatheringI = TheGatheringI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

theGathering :: Difficulty -> Scenario
theGathering = TheGathering . TheGatheringI . baseAttrs "01104" "The Gathering"

newtype TheMidnightMasksI = TheMidnightMasksI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

theMidnightMasks :: Difficulty -> Scenario
theMidnightMasks = TheMidnightMasks . TheMidnightMasksI . baseAttrs "01120" "The Midnight Masks"

instance (HasCount EnemyCount (CurrentInvestigatorLocation, [Trait]) env, HasQueue env) => RunMessage env Scenario where
  runMessage msg = \case
    TheGathering x -> TheGathering <$> runMessage msg x
    TheMidnightMasks x -> TheMidnightMasks <$> runMessage msg x

instance (HasCount EnemyCount (CurrentInvestigatorLocation, [Trait]) env, HasQueue env) => RunMessage env TheGatheringI where
  runMessage msg s@(TheGatheringI Attrs {..}) = case msg of
    Setup -> s <$ traverse_
      pushMessage
      [ PlaceLocation "01111"
      , RevealLocation "01111"
      , MoveAllTo "01111"
      , BeginInvestigation
      ]
    ResolveToken Token.Skull iid skillValue ->
      if scenarioDifficulty `elem` [Easy, Standard]
         then do
           ghoulCount <- unEnemyCount <$> asks (getCount (CurrentInvestigatorLocation, [Ghoul]))
           s <$ runCheck (skillValue - ghoulCount)
         else do
           unshiftMessage (AddOnFailure $ FindAndDrawEncounterCard iid (EnemyType, [Ghoul]))
           s <$ runCheck (skillValue - 2)
    ResolveToken Token.Cultist iid skillValue ->
      if scenarioDifficulty `elem` [Easy, Standard]
         then do
           unshiftMessage (AddOnFailure $ InvestigatorDamage iid (TokenSource Token.Cultist) 0 1)
           s <$ runCheck (skillValue - 1)
         else do
           unshiftMessage (DrawAnotherToken iid skillValue Token.Cultist)
           unshiftMessage (AddOnFailure $ InvestigatorDamage iid (TokenSource Token.Cultist) 0 2)
           pure s
    ResolveToken Token.Tablet iid skillValue -> do
      ghoulCount <- unEnemyCount <$> asks (getCount (CurrentInvestigatorLocation, [Ghoul]))
      if scenarioDifficulty `elem` [Easy, Standard]
         then do
           when (ghoulCount > 0) $ unshiftMessage (InvestigatorDamage iid (TokenSource Token.Tablet) 1 0)
           s <$ runCheck (skillValue - 2)
         else do
           when (ghoulCount > 0) $ unshiftMessage (InvestigatorDamage iid (TokenSource Token.Tablet) 1 1)
           s <$ runCheck (skillValue - 4)
    _ -> pure s

instance (HasQueue env) => RunMessage env TheMidnightMasksI where
  runMessage _ = pure
