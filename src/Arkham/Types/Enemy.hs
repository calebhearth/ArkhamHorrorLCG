{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy
  ( lookupEnemy
  , Enemy
  )
where

import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.InvestigatorId
import Arkham.Types.Keyword (Keyword)
import qualified Arkham.Types.Keyword as Keyword
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson
import Data.Coerce
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Lens.Micro
import Safe (fromJustNote)

lookupEnemy :: CardCode -> (EnemyId -> Enemy)
lookupEnemy = fromJustNote "Unkown enemy" . flip HashMap.lookup allEnemies

allEnemies :: HashMap CardCode (EnemyId -> Enemy)
allEnemies = HashMap.fromList
  [ ("01116", ghoulPriest)
  , ("01118", fleshEater)
  , ("01119", icyGhoul)
  , ("01159", swarmOfRats)
  , ("01160", ghoulMinion)
  , ("01161", ravenousGhoul)
  ]

instance HasCardCode Enemy where
  getCardCode = enemyCardCode . enemyAttrs

instance HasTraits Enemy where
  traitsOf = enemyTraits . enemyAttrs

data Attrs = Attrs
  { enemyName :: Text
  , enemyId :: EnemyId
  , enemyCardCode :: CardCode
  , enemyEngagedInvestigators :: HashSet InvestigatorId
  , enemyLocation :: LocationId
  , enemyFight :: Int
  , enemyHealth :: GameValue
  , enemyEvade :: Int
  , enemyDamage :: Int
  , enemyHealthDamage :: Int
  , enemySanityDamage :: Int
  , enemyTraits :: HashSet Trait
  , enemyVictory :: Maybe Int
  , enemyKeywords :: HashSet Keyword
  , enemyPrey :: Prey
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

engagedInvestigators :: Lens' Attrs (HashSet InvestigatorId)
engagedInvestigators =
  lens enemyEngagedInvestigators $ \m x -> m { enemyEngagedInvestigators = x }

location :: Lens' Attrs LocationId
location = lens enemyLocation $ \m x -> m { enemyLocation = x }

damage :: Lens' Attrs Int
damage = lens enemyDamage $ \m x -> m { enemyDamage = x }

health :: Lens' Attrs GameValue
health = lens enemyHealth $ \m x -> m { enemyHealth = x }

data Enemy
  = GhoulPriest GhoulPriestI
  | FleshEater FleshEaterI
  | IcyGhoul IcyGhoulI
  | SwarmOfRats SwarmOfRatsI
  | GhoulMinion GhoulMinionI
  | RavenousGhoul RavenousGhoulI
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

enemyAttrs :: Enemy -> Attrs
enemyAttrs = \case
  GhoulPriest attrs -> coerce attrs
  FleshEater attrs -> coerce attrs
  IcyGhoul attrs -> coerce attrs
  SwarmOfRats attrs -> coerce attrs
  GhoulMinion attrs -> coerce attrs
  RavenousGhoul attrs -> coerce attrs

baseAttrs :: EnemyId -> CardCode -> Attrs
baseAttrs eid cardCode =
  let
    MkEncounterCard {..} =
      fromJustNote "missing encounter card"
        $ HashMap.lookup cardCode allEncounterCards
  in
    Attrs
      { enemyName = ecName
      , enemyId = eid
      , enemyCardCode = cardCode
      , enemyEngagedInvestigators = mempty
      , enemyLocation = "00000" -- no known location
      , enemyFight = 1
      , enemyHealth = Static 1
      , enemyEvade = 1
      , enemyDamage = 0
      , enemyHealthDamage = 0
      , enemySanityDamage = 0
      , enemyTraits = HashSet.fromList ecTraits
      , enemyVictory = Nothing
      , enemyKeywords = HashSet.fromList ecKeywords
      , enemyPrey = AnyPrey
      }

newtype GhoulPriestI = GhoulPriestI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

ghoulPriest :: EnemyId -> Enemy
ghoulPriest uuid = GhoulPriest $ GhoulPriestI $ (baseAttrs uuid "01116")
  { enemyHealthDamage = 2
  , enemySanityDamage = 2
  , enemyFight = 4
  , enemyHealth = PerPlayer 5
  , enemyEvade = 4
  , enemyVictory = Just 2
  , enemyPrey = HighestSkill SkillCombat
  }

newtype FleshEaterI = FleshEaterI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

fleshEater :: EnemyId -> Enemy
fleshEater uuid = FleshEater $ FleshEaterI $ (baseAttrs uuid "01118")
  { enemyHealthDamage = 1
  , enemySanityDamage = 2
  , enemyFight = 4
  , enemyHealth = Static 4
  , enemyEvade = 1
  , enemyVictory = Just 1
  }

newtype IcyGhoulI = IcyGhoulI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

icyGhoul :: EnemyId -> Enemy
icyGhoul uuid = IcyGhoul $ IcyGhoulI $ (baseAttrs uuid "01119")
  { enemyHealthDamage = 2
  , enemySanityDamage = 1
  , enemyFight = 3
  , enemyHealth = Static 4
  , enemyEvade = 4
  , enemyVictory = Just 1
  }


newtype SwarmOfRatsI = SwarmOfRatsI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

swarmOfRats :: EnemyId -> Enemy
swarmOfRats uuid = SwarmOfRats $ SwarmOfRatsI $ (baseAttrs uuid "01159")
  { enemyHealthDamage = 1
  , enemyEvade = 3
  }

newtype GhoulMinionI = GhoulMinionI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

ghoulMinion :: EnemyId -> Enemy
ghoulMinion uuid = GhoulMinion $ GhoulMinionI $ (baseAttrs uuid "01160")
  { enemyHealthDamage = 1
  , enemySanityDamage = 1
  , enemyFight = 2
  , enemyHealth = Static 2
  , enemyEvade = 2
  }

newtype RavenousGhoulI = RavenousGhoulI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

ravenousGhoul :: EnemyId -> Enemy
ravenousGhoul uuid = RavenousGhoul $ RavenousGhoulI $ (baseAttrs uuid "01160")
  { enemyHealthDamage = 1
  , enemySanityDamage = 1
  , enemyFight = 3
  , enemyHealth = Static 3
  , enemyEvade = 3
  , enemyPrey = LowestHealth
  }

type EnemyRunner env
  = ( HasSet LocationId () env
    , HasCount PlayerCount () env
    , HasQueue env
    , HasSet ClosestLocationId (LocationId, Prey) env
    )

instance (EnemyRunner env) => RunMessage env Enemy where
  runMessage msg = \case
    GhoulPriest x -> GhoulPriest <$> runMessage msg x
    FleshEater x -> FleshEater <$> runMessage msg x
    IcyGhoul x -> IcyGhoul <$> runMessage msg x
    SwarmOfRats x -> SwarmOfRats <$> runMessage msg x
    GhoulMinion x -> GhoulMinion <$> runMessage msg x
    RavenousGhoul x -> RavenousGhoul <$> runMessage msg x

spawnAt
  :: (MonadIO m, HasSet LocationId () env, MonadReader env m, HasQueue env)
  => LocationId
  -> EnemyId
  -> m ()
spawnAt lid eid = do
  locations <- asks (getSet ())
  if lid `elem` locations
    then unshiftMessage (EnemySpawn lid eid)
    else unshiftMessage (RemoveEnemy eid)

instance (EnemyRunner env) => RunMessage env GhoulPriestI where
  runMessage msg (GhoulPriestI attrs) = GhoulPriestI <$> runMessage msg attrs

instance (EnemyRunner env) => RunMessage env FleshEaterI where
  runMessage msg e@(FleshEaterI attrs@Attrs {..}) = case msg of
    InvestigatorDrawEnemy _ _ eid | eid == enemyId ->
      e <$ spawnAt "01113" enemyId
    _ -> FleshEaterI <$> runMessage msg attrs

instance (EnemyRunner env) => RunMessage env IcyGhoulI where
  runMessage msg e@(IcyGhoulI attrs@Attrs {..}) = case msg of
    InvestigatorDrawEnemy _ _ eid | eid == enemyId ->
      e <$ spawnAt "01114" enemyId
    _ -> IcyGhoulI <$> runMessage msg attrs

instance (EnemyRunner env) => RunMessage env SwarmOfRatsI where
  runMessage msg (SwarmOfRatsI attrs) = SwarmOfRatsI <$> runMessage msg attrs

instance (EnemyRunner env) => RunMessage env GhoulMinionI where
  runMessage msg (GhoulMinionI attrs) = GhoulMinionI <$> runMessage msg attrs

instance (EnemyRunner env) => RunMessage env RavenousGhoulI where
  runMessage msg (RavenousGhoulI attrs) =
    RavenousGhoulI <$> runMessage msg attrs

instance (EnemyRunner env) => RunMessage env Attrs where
  runMessage msg a@Attrs {..} = case msg of
    EnemySpawn lid eid | eid == enemyId -> pure $ a & location .~ lid
    HuntersMove
      | Keyword.Hunter `elem` enemyKeywords && null enemyEngagedInvestigators -> do
        closestLocationIds <-
          HashSet.toList . HashSet.map unClosestLocationId <$> asks
            (getSet (enemyLocation, AnyPrey))
        case closestLocationIds of
          [] -> pure a
          [lid] -> a <$ unshiftMessage (EnemyMove enemyId enemyLocation lid)
          ls -> a <$ unshiftMessage
            (Ask $ ChooseOne $ map
              (ChoiceResult . EnemyMove enemyId enemyLocation)
              ls
            )
    EnemiesAttack | not (null enemyEngagedInvestigators) -> do
      unshiftMessages $ map (flip EnemyWillAttack enemyId) $ HashSet.toList
        enemyEngagedInvestigators
      pure a
    AttackEnemy iid eid skillType baseSkillValue damageAmount
      | eid == enemyId -> do
        let
          onFailure = if Keyword.Retaliate `elem` enemyKeywords
            then [EnemyAttack iid eid]
            else []
        a <$ unshiftMessage
          (BeginSkillCheck
            iid
            skillType
            enemyFight
            baseSkillValue
            [EnemyDamage eid (InvestigatorSource iid) damageAmount]
            onFailure
          )
    EnemyAttack iid eid | eid == enemyId -> a <$ unshiftMessage
      (InvestigatorAssignDamage iid enemyId enemyHealthDamage enemySanityDamage)
    EnemyDamage eid source amount | eid == enemyId -> do
      playerCount <- unPlayerCount <$> asks (getCount ())
      (a & damage +~ amount) <$ when
        (a ^. damage + amount >= a ^. health . to (`fromGameValue` playerCount))
        (unshiftMessage (EnemyDefeated eid source))
    EnemyEngageInvestigator eid iid | eid == enemyId ->
      pure $ a & engagedInvestigators %~ HashSet.insert iid
    CheckAttackOfOpportunity iid | iid `elem` enemyEngagedInvestigators ->
      a <$ unshiftMessage (EnemyWillAttack iid enemyId)
    InvestigatorDrawEnemy _ lid eid | eid == enemyId -> do
      unshiftMessage (EnemySpawn lid eid)
      pure $ a & location .~ lid
    _ -> pure a
