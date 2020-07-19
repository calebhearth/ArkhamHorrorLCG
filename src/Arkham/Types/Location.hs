{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location (lookupLocation, Location(..)) where

import           Arkham.Types.Classes
import           Arkham.Types.EnemyId
import           Arkham.Types.InvestigatorId
import           Arkham.Types.LocationId
import           Arkham.Types.Message
import           ClassyPrelude
import           Data.Aeson
import           Data.Coerce
import qualified Data.HashMap.Strict         as HashMap
import qualified Data.HashSet                as HashSet
import           Lens.Micro
import           Safe                        (fromJustNote)

lookupLocation :: LocationId -> Location
lookupLocation lid = fromJustNote ("Unkown location: " <> show lid) $ HashMap.lookup lid allLocations

allLocations :: HashMap LocationId Location
allLocations = HashMap.fromList $ map (\s -> (locationId . locationAttrs $ s, s)) [study, hallway]

data GameValue = Static Int
    | PerPlayer Int
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Attrs = Attrs
    { locationName          :: Text
    , locationId            :: LocationId
    , locationRevealClues   :: GameValue
    , locationClues         :: Int
    , locationShroud        :: Int
    , locationRevealed      :: Bool
    , locationInvestigators :: HashSet InvestigatorId
    , locationEnemies       :: HashSet EnemyId
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance HasClueCount Location where
  getClueCount = ClueCount . locationClues . locationAttrs

investigators :: Lens' Attrs (HashSet InvestigatorId)
investigators = lens locationInvestigators $ \m x -> m { locationInvestigators = x }

baseAttrs :: LocationId -> Text -> Int -> GameValue -> Attrs
baseAttrs lid name shroud revealClues = Attrs
  { locationName = name
  , locationId = lid
  , locationRevealClues = revealClues
  , locationClues = 0
  , locationShroud = shroud
  , locationRevealed = False
  , locationInvestigators = mempty
  , locationEnemies = mempty
  }

clues :: Lens' Attrs Int
clues = lens locationClues $ \m x -> m { locationClues = x }

fromGameValue :: GameValue -> Int -> Int
fromGameValue (Static n) _     = n
fromGameValue (PerPlayer n) pc = n * pc

revealed :: Lens' Attrs Bool
revealed = lens locationRevealed $ \m x -> m { locationRevealed = x }

enemies :: Lens' Attrs (HashSet EnemyId)
enemies = lens locationEnemies $ \m x -> m { locationEnemies = x }

data Location = Study StudyI
    | Hallway HallwayI
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

locationAttrs :: Location -> Attrs
locationAttrs = \case
  Study attrs -> coerce attrs
  Hallway attrs -> coerce attrs

newtype StudyI = StudyI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

study :: Location
study = Study $ StudyI $ baseAttrs "01111" "Study" 2 (PerPlayer 2)

newtype HallwayI = HallwayI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

hallway :: Location
hallway = Hallway $ HallwayI $ baseAttrs "01112" "Hallway" 1 (Static 0)

instance (HasSet InvestigatorId env, HasQueue env) => RunMessage env Location where
  runMessage msg = \case
    Study x -> Study <$> runMessage msg x
    Hallway x -> Hallway <$> runMessage msg x

instance (HasSet InvestigatorId env, HasQueue env) => RunMessage env StudyI where
  runMessage msg (StudyI attrs)  = StudyI <$> runMessage msg attrs

instance (HasSet InvestigatorId env, HasQueue env) => RunMessage env HallwayI where
  runMessage msg (HallwayI attrs) = HallwayI <$> runMessage msg attrs

instance (HasSet InvestigatorId env, HasQueue env) => RunMessage env Attrs where
  runMessage msg a@Attrs {..} = case msg of
    DiscoverClueAtLocation iid lid | lid == locationId ->
      if locationClues > 0
        then do
          unshiftMessage (DiscoverClue iid)
          pure $ a & clues -~ 1
        else pure a
    MoveTo iid lid | lid == locationId -> pure $ a & investigators %~ HashSet.insert iid
    EnemySpawn lid eid | lid == locationId ->
      pure $ a & enemies %~ HashSet.insert eid
    EnemyDefeated eid _ ->
      pure $ a & enemies %~ HashSet.delete eid
    RevealLocation lid | lid == locationId -> do
      clueCount <- fromGameValue locationRevealClues <$> asks (HashSet.size . getSet @InvestigatorId)
      pure $ a & clues .~ clueCount & revealed .~ True
    _ -> pure a
