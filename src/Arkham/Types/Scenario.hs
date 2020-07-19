{-# LANGUAGE FlexibleContexts #-}
module Arkham.Types.Scenario
  ( lookupScenario, Scenario )
where

import           Arkham.Types.Classes
import           Arkham.Types.Message
import           Arkham.Types.ScenarioId
import           ClassyPrelude
import           Data.Aeson
import           Data.Coerce
import qualified Data.HashMap.Strict     as HashMap
import           Safe                    (fromJustNote)

lookupScenario :: ScenarioId -> Scenario
lookupScenario = fromJustNote "Unkown scenario" . flip HashMap.lookup allScenarios

allScenarios :: HashMap ScenarioId Scenario
allScenarios = HashMap.fromList $ map (\s -> (scenarioId . scenarioAttrs $ s, s)) [theGathering, theMidnightMasks]

data Attrs = Attrs
    { scenarioName :: Text
    , scenarioId   :: ScenarioId
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Scenario = TheGathering TheGatheringI
    | TheMidnightMasks TheMidnightMasksI
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

scenarioAttrs :: Scenario -> Attrs
scenarioAttrs = \case
  TheGathering attrs -> coerce attrs
  TheMidnightMasks attrs -> coerce attrs

newtype TheGatheringI = TheGatheringI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

theGathering :: Scenario
theGathering = TheGathering $ TheGatheringI $ Attrs { scenarioName = "The Gathering" , scenarioId = "01104" }

newtype TheMidnightMasksI = TheMidnightMasksI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

theMidnightMasks :: Scenario
theMidnightMasks = TheMidnightMasks $ TheMidnightMasksI $ Attrs { scenarioName = "The Midnight Masks" , scenarioId = "01120" }

instance (HasQueue env) => RunMessage env Scenario where
  runMessage msg = \case
    TheGathering x -> TheGathering <$> runMessage msg x
    TheMidnightMasks x -> TheMidnightMasks <$> runMessage msg x

instance (HasQueue env) => RunMessage env TheGatheringI where
  runMessage Setup s = s <$ traverse_
    pushMessage
    [ PlaceLocation "01111"
    , RevealLocation "01111"
    , MoveAllTo "01111"
    , BeginInvestigation
    ]
  runMessage _ s = pure s

instance (HasQueue env) => RunMessage env TheMidnightMasksI where
  runMessage _ = pure
