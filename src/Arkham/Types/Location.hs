{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location
  ( lookupLocation
  , isBlocked
  , Location(..)
  )
where

import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Stats (Stats)
import qualified Arkham.Types.Stats as Stats
import Arkham.Types.Trait
import Arkham.Types.TreacheryId
import ClassyPrelude
import Data.Aeson
import Data.Coerce
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Lens.Micro
import Safe (fromJustNote)

lookupLocation :: LocationId -> Location
lookupLocation lid =
  fromJustNote ("Unkown location: " <> show lid)
    $ HashMap.lookup lid allLocations

isBlocked :: Location -> Bool
isBlocked = locationBlocked . locationAttrs

allLocations :: HashMap LocationId Location
allLocations = HashMap.fromList $ map
  (\s -> (locationId . locationAttrs $ s, s))
  [study, hallway, attic, cellar, parlor]

data Attrs = Attrs
  { locationName               :: Text
  , locationId                 :: LocationId
  , locationRevealClues        :: GameValue
  , locationClues              :: Int
  , locationShroud             :: Int
  , locationRevealed           :: Bool
  , locationBlocked            :: Bool
  , locationInvestigators      :: HashSet InvestigatorId
  , locationEnemies            :: HashSet EnemyId
  , locationVictory            :: Maybe Int
  , locationSymbol             :: LocationSymbol
  , locationConnectedSymbols   :: HashSet LocationSymbol
  , locationConnectedLocations :: HashSet LocationId
  , locationTraits             :: HashSet Trait
  , locationTreacheries :: HashSet TreacheryId
  , locationAssets :: HashSet AssetId
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HasClueCount Location where
  getClueCount = ClueCount . locationClues . locationAttrs

instance HasSet EnemyId () Location where
  getSet _ = locationEnemies . locationAttrs

instance HasSet InvestigatorId () Location where
  getSet _ = locationInvestigators . locationAttrs

instance HasSet ConnectedLocationId () Location where
  getSet _ =
    HashSet.map ConnectedLocationId . locationConnectedLocations . locationAttrs

investigators :: Lens' Attrs (HashSet InvestigatorId)
investigators =
  lens locationInvestigators $ \m x -> m { locationInvestigators = x }

treacheries :: Lens' Attrs (HashSet TreacheryId)
treacheries = lens locationTreacheries $ \m x -> m { locationTreacheries = x }

assets :: Lens' Attrs (HashSet AssetId)
assets = lens locationAssets $ \m x -> m { locationAssets = x }

connectedLocations :: Lens' Attrs (HashSet LocationId)
connectedLocations =
  lens locationConnectedLocations $ \m x -> m { locationConnectedLocations = x }

blocked :: Lens' Attrs Bool
blocked = lens locationBlocked $ \m x -> m { locationBlocked = x }

baseAttrs
  :: LocationId
  -> Text
  -> Int
  -> GameValue
  -> LocationSymbol
  -> [LocationSymbol]
  -> Attrs
baseAttrs lid name shroud' revealClues symbol' connectedSymbols' = Attrs
  { locationName = name
  , locationId = lid
  , locationRevealClues = revealClues
  , locationClues = 0
  , locationShroud = shroud'
  , locationRevealed = False
  , locationBlocked = False
  , locationInvestigators = mempty
  , locationEnemies = mempty
  , locationVictory = Nothing
  , locationSymbol = symbol'
  , locationConnectedSymbols = HashSet.fromList connectedSymbols'
  , locationConnectedLocations = mempty
  , locationTraits = mempty
  , locationTreacheries = mempty
  , locationAssets = mempty
  }

clues :: Lens' Attrs Int
clues = lens locationClues $ \m x -> m { locationClues = x }

shroud :: Lens' Attrs Int
shroud = lens locationShroud $ \m x -> m { locationShroud = x }

revealed :: Lens' Attrs Bool
revealed = lens locationRevealed $ \m x -> m { locationRevealed = x }

enemies :: Lens' Attrs (HashSet EnemyId)
enemies = lens locationEnemies $ \m x -> m { locationEnemies = x }

data Location
  = Study StudyI
  | Hallway HallwayI
  | Attic AtticI
  | Cellar CellarI
  | Parlor ParlorI
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

locationAttrs :: Location -> Attrs
locationAttrs = \case
  Study attrs -> coerce attrs
  Hallway attrs -> coerce attrs
  Attic attrs -> coerce attrs
  Cellar attrs -> coerce attrs
  Parlor attrs -> coerce attrs

newtype StudyI = StudyI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

study :: Location
study = Study $ StudyI $ baseAttrs "01111" "Study" 2 (PerPlayer 2) Circle []

newtype HallwayI = HallwayI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

hallway :: Location
hallway = Hallway $ HallwayI $ baseAttrs
  "01112"
  "Hallway"
  1
  (Static 0)
  Square
  [Triangle, Plus, Diamond]

newtype AtticI = AtticI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

attic :: Location
attic =
  Attic $ AtticI $ (baseAttrs "01113" "Attic" 1 (PerPlayer 2) Triangle [Square])
    { locationVictory = Just 1
    }

newtype CellarI = CellarI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

cellar :: Location
cellar =
  Cellar $ CellarI $ (baseAttrs "01114" "Cellar" 4 (PerPlayer 2) Plus [Square])
    { locationVictory = Just 1
    }

newtype ParlorI = ParlorI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

parlor :: Location
parlor =
  Parlor $ ParlorI $ (baseAttrs "01115" "Parlor" 2 (Static 0) Diamond [Square])
    { locationBlocked = True
    }

type LocationRunner env
  = ( HasCount PlayerCount () env
    , HasQueue env
    , HasId StoryAssetId CardCode env
    , HasInvestigatorStats Stats InvestigatorId env
    , HasId (Maybe OwnerId) AssetId env
    )

instance (LocationRunner env) => RunMessage env Location where
  runMessage msg = \case
    Study x -> Study <$> runMessage msg x
    Hallway x -> Hallway <$> runMessage msg x
    Attic x -> Attic <$> runMessage msg x
    Cellar x -> Cellar <$> runMessage msg x
    Parlor x -> Parlor <$> runMessage msg x

instance (LocationRunner env) => RunMessage env StudyI where
  runMessage msg (StudyI attrs) = StudyI <$> runMessage msg attrs

instance (LocationRunner env) => RunMessage env HallwayI where
  runMessage msg (HallwayI attrs) = HallwayI <$> runMessage msg attrs

instance (LocationRunner env) => RunMessage env AtticI where
  runMessage msg a@(AtticI attrs@Attrs {..}) = case msg of
    AfterEnterLocation iid lid | lid == locationId -> do
      unshiftMessage (InvestigatorDamage iid (LocationSource locationId) 0 1)
      pure a
    _ -> AtticI <$> runMessage msg attrs

instance (LocationRunner env) => RunMessage env CellarI where
  runMessage msg a@(CellarI attrs@Attrs {..}) = case msg of
    AfterEnterLocation iid lid | lid == locationId -> do
      unshiftMessage (InvestigatorDamage iid (LocationSource locationId) 1 0)
      pure a
    _ -> CellarI <$> runMessage msg attrs

instance (LocationRunner env) => RunMessage env ParlorI where
  runMessage msg l@(ParlorI attrs@Attrs {..}) = case msg of
    RevealLocation lid | lid == locationId -> do
      attrs' <- runMessage msg attrs
      pure $ ParlorI $ attrs' & blocked .~ False
    PrePlayerWindow | locationRevealed -> do
      aid <- unStoryAssetId <$> asks (getId (CardCode "01117"))
      miid <- fmap unOwnerId <$> asks (getId aid)
      case miid of
        Just _ ->
          l <$ unshiftMessage (RemoveAbilitiesFrom (LocationSource locationId))
        Nothing -> l <$ unshiftMessages
          [ RemoveAbilitiesFrom (LocationSource locationId)
          , AddAbility (AssetSource aid) (LocationSource locationId, 2)
          ]
    UseCardAbility iid (LocationSource lid) 1
      | lid == locationId && locationRevealed -> l
      <$ unshiftMessage (Resign iid)
    UseCardAbility iid (LocationSource lid) 2
      | lid == locationId && locationRevealed -> do
        aid <- unStoryAssetId <$> asks (getId (CardCode "01117"))
        skillValue <- Stats.intellect <$> asks (getStats iid)
        l <$ unshiftMessage
          (BeginSkillCheck
            iid
            SkillIntellect
            4
            skillValue
            [TakeControlOfAsset iid aid]
            []
          )
    _ -> ParlorI <$> runMessage msg attrs

instance (LocationRunner env) => RunMessage env Attrs where
  runMessage msg a@Attrs {..} = case msg of
    Investigate skillType skillValue iid lid | lid == locationId ->
      a <$ unshiftMessage
        (BeginSkillCheck
          iid
          skillType
          locationShroud
          skillValue
          [SuccessfulInvestigation lid, DiscoverClueAtLocation iid lid]
          []
        )
    PlacedLocation lid | lid == locationId ->
      a <$ unshiftMessage (AddConnection lid locationSymbol)
    AttachTreacheryToLocation tid lid | lid == locationId ->
      pure $ a & treacheries %~ HashSet.insert tid
    AddAssetAt aid lid | lid == locationId ->
      pure $ a & assets %~ HashSet.insert aid
    LocationIncreaseShroud lid n | lid == locationId -> pure $ a & shroud +~ n
    LocationDecreaseShroud lid n | lid == locationId -> pure $ a & shroud -~ n
    AddConnection lid symbol' | symbol' `elem` locationConnectedSymbols -> do
      unshiftMessages
        [ AddConnectionBack locationId locationSymbol
        , AddedConnection locationId lid
        ]
      pure $ a & connectedLocations %~ HashSet.insert lid
    AddConnectionBack lid symbol' | symbol' `elem` locationConnectedSymbols ->
      do
        unshiftMessage (AddedConnection locationId lid)
        pure $ a & connectedLocations %~ HashSet.insert lid
    DiscoverClueAtLocation iid lid | lid == locationId -> if locationClues > 0
      then do
        unshiftMessage (DiscoverClue iid)
        pure $ a & clues -~ 1
      else pure a
    WhenEnterLocation iid lid | lid == locationId -> do
      unless locationRevealed $ unshiftMessage (RevealLocation lid)
      pure $ a & investigators %~ HashSet.insert iid
    EnemySpawn lid eid | lid == locationId ->
      pure $ a & enemies %~ HashSet.insert eid
    RemoveEnemy eid -> pure $ a & enemies %~ HashSet.delete eid
    EnemyDefeated eid _ _ -> pure $ a & enemies %~ HashSet.delete eid
    RevealLocation lid | lid == locationId -> do
      clueCount <- fromGameValue locationRevealClues . unPlayerCount <$> asks
        (getCount ())
      pure $ a & clues .~ clueCount & revealed .~ True
    _ -> pure a
