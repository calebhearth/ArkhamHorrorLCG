{-# LANGUAGE FlexibleContexts #-}
module Arkham.Types.Enemy
  ( lookupEnemy, Enemy )
where

import           Arkham.Types.Card
import           Arkham.Types.Classes
import           Arkham.Types.EnemyId
import           Arkham.Types.InvestigatorId
import           Arkham.Types.LocationId
import           Arkham.Types.Message
import           ClassyPrelude
import           Data.Aeson
-- import           Data.Coerce
import qualified Data.HashMap.Strict         as HashMap
import qualified Data.HashSet                as HashSet
import           Lens.Micro
import           Safe                        (fromJustNote)

lookupEnemy :: CardCode -> (EnemyId -> Enemy)
lookupEnemy = fromJustNote "Unkown enemy" . flip HashMap.lookup allEnemies

allEnemies :: HashMap CardCode (EnemyId -> Enemy)
allEnemies = HashMap.fromList
  [ ("01159", swarmOfRats)
  , ("01160", ghoulMinion)
  ]

data Attrs = Attrs
    { enemyName                 :: Text
    , enemyId                   :: EnemyId
    , enemyCardCode             :: CardCode
    , enemyEngagedInvestigators :: HashSet InvestigatorId
    , enemyLocation             :: LocationId
    , enemyHealthDamage         :: Int
    , enemySanityDamage         :: Int
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

engagedInvestigators :: Lens' Attrs (HashSet InvestigatorId)
engagedInvestigators = lens enemyEngagedInvestigators $ \m x -> m { enemyEngagedInvestigators = x }

location :: Lens' Attrs LocationId
location = lens enemyLocation $ \m x -> m { enemyLocation = x }

data Enemy = SwarmOfRats SwarmOfRatsI
    | GhoulMinion GhoulMinionI
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- enemyAttrs :: Enemy -> Attrs
-- enemyAttrs = \case
--   SwarmOfRats attrs -> coerce attrs
--   GhoulMinion attrs -> coerce attrs

baseAttrs :: EnemyId -> CardCode -> Text -> Attrs
baseAttrs eid cardCode name = Attrs
  { enemyName = name
  , enemyId = eid
  , enemyCardCode = cardCode
  , enemyEngagedInvestigators = mempty
  , enemyLocation = "00000" -- no known location
  , enemyHealthDamage = 0
  , enemySanityDamage = 0
  }

newtype SwarmOfRatsI = SwarmOfRatsI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

swarmOfRats :: EnemyId -> Enemy
swarmOfRats uuid = SwarmOfRats $ SwarmOfRatsI $ (baseAttrs uuid "01159" "Swarm of Rats") { enemyHealthDamage = 1 }

newtype GhoulMinionI = GhoulMinionI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

ghoulMinion :: EnemyId -> Enemy
ghoulMinion uuid = GhoulMinion $ GhoulMinionI $ (baseAttrs uuid "01160" "Ghoul Minion") { enemyHealthDamage = 1, enemySanityDamage = 1 }

instance (HasQueue env) => RunMessage env Enemy where
  runMessage msg = \case
    SwarmOfRats x -> SwarmOfRats <$> runMessage msg x
    GhoulMinion x -> GhoulMinion <$> runMessage msg x

instance (HasQueue env) => RunMessage env SwarmOfRatsI where
  runMessage msg (SwarmOfRatsI attrs) = SwarmOfRatsI <$> runMessage msg attrs

instance (HasQueue env) => RunMessage env GhoulMinionI where
  runMessage msg (GhoulMinionI attrs) = GhoulMinionI <$> runMessage msg attrs

instance (HasQueue env) => RunMessage env Attrs where
  runMessage msg a@Attrs {..} = case msg of
    EnemyAttack iid eid | eid == enemyId ->
      a <$ unshiftMessage (InvestigatorAssignDamage iid enemyId enemyHealthDamage enemySanityDamage)
    EnemyEngageInvestigator eid iid | eid == enemyId -> pure $ a & engagedInvestigators %~ HashSet.insert iid
    CheckAttackOfOpportunity iid | iid `elem` enemyEngagedInvestigators -> a <$ unshiftMessage (EnemyWillAttack iid enemyId)
    InvestigatorDrawEnemy _ lid eid | eid == enemyId -> do
      unshiftMessage (EnemySpawn lid eid)
      pure $ a & location .~ lid
    _ -> pure a
