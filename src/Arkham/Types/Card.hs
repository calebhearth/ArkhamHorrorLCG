module Arkham.Types.Card
  ( CardCode(..)
  , Card(..)
  , PlayerCard(..)
  , EncounterCard(..)
  , PlayerCardType(..)
  , EncounterCardType(..)
  , ClassSymbol(..)
  , HasCardCode(..)
  , HasCost(..)
  , allCards
  , allPlayerCards
  , allEncounterCards
  )
where

import Arkham.Types.Keyword (Keyword)
import qualified Arkham.Types.Keyword as Keyword
import Arkham.Types.SkillType
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson
import qualified Data.HashMap.Strict as HashMap

class HasCardCode a where
  getCardCode :: a -> CardCode

class HasCost a where
  getCost :: a -> Int

newtype CardCode = CardCode { unCardCode :: Text }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, IsString)

data PlayerCardType
  = AssetType
  | EventType
  | SkillType
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data EncounterCardType
  = TreacheryType
  | EnemyType
  | LocationType
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ClassSymbol
  = Guardian
  | Seeker
  | Survivor
  | Rogue
  | Mystic
  | Neutral
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Card
  = PlayerCard PlayerCard
  | EncounterCard EncounterCard
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HasCardCode Card where
  getCardCode (PlayerCard card) = getCardCode card
  getCardCode (EncounterCard card) = getCardCode card

instance HasCost Card where
  getCost (PlayerCard card) = getCost card
  getCost _ = 0

data PlayerCard = MkPlayerCard
  { pcCardCode   :: CardCode
  , pcName :: Text
  , pcCost       :: Int
  , pcLevel      :: Int
  , pcCardType   :: PlayerCardType
  , pcClassSybol :: ClassSymbol
  , pcSkills     :: [SkillType]
  , pcTraits     :: [Trait]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HasCardCode PlayerCard where
  getCardCode = pcCardCode

instance HasCost PlayerCard where
  getCost = pcCost

data EncounterCard = MkEncounterCard
  { ecCardCode :: CardCode
  , ecName :: Text
  , ecCardType :: EncounterCardType
  , ecTraits   :: [Trait]
  , ecKeywords   :: [Keyword]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HasCardCode EncounterCard where
  getCardCode = ecCardCode

allCards :: HashMap CardCode Card
allCards =
  HashMap.map PlayerCard allPlayerCards
    <> HashMap.map EncounterCard allEncounterCards

allPlayerCards :: HashMap CardCode PlayerCard
allPlayerCards = HashMap.fromList $ map
  (\c -> (getCardCode c, c))
  [ MkPlayerCard
    { pcCardCode = "01020"
    , pcName = "Machete"
    , pcCost = 3
    , pcLevel = 0
    , pcCardType = AssetType
    , pcClassSybol = Guardian
    , pcSkills = [SkillCombat]
    , pcTraits = [Item, Weapon, Melee]
    }
  , MkPlayerCard
    { pcCardCode = "01021"
    , pcName = "Guard Dog"
    , pcCost = 3
    , pcLevel = 0
    , pcCardType = AssetType
    , pcClassSybol = Guardian
    , pcSkills = [SkillCombat]
    , pcTraits = [Ally, Creature]
    }
  , MkPlayerCard
    { pcCardCode = "01088"
    , pcName = "Emergency Cache"
    , pcCost = 0
    , pcLevel = 0
    , pcCardType = EventType
    , pcClassSybol = Neutral
    , pcSkills = []
    , pcTraits = [Supply]
    }
  ]

allEncounterCards :: HashMap CardCode EncounterCard
allEncounterCards = HashMap.fromList $ map
  (\c -> (getCardCode c, c))
  [ MkEncounterCard
    { ecCardCode = "01116"
    , ecName = "Ghoul Priest"
    , ecCardType = EnemyType
    , ecTraits = [Humanoid, Monster, Ghoul, Elite]
    , ecKeywords = [Keyword.Hunter, Keyword.Retaliate]
    }
  , MkEncounterCard
    { ecCardCode = "01159"
    , ecName = "Swarm of Rats"
    , ecCardType = EnemyType
    , ecTraits = [Creature]
    , ecKeywords = [Keyword.Hunter]
    }
  , MkEncounterCard
    { ecCardCode = "01160"
    , ecName = "Ghoul Minion"
    , ecCardType = EnemyType
    , ecTraits = [Humanoid, Monster, Ghoul]
    , ecKeywords = mempty
    }
  , MkEncounterCard
    { ecCardCode = "01161"
    , ecName = "Ravenous Ghoul"
    , ecCardType = EnemyType
    , ecTraits = [Humanoid, Monster, Ghoul]
    , ecKeywords = mempty
    }
  , MkEncounterCard
    { ecCardCode = "01162"
    , ecName = "Grasping Hands"
    , ecCardType = TreacheryType
    , ecTraits = [Hazard]
    , ecKeywords = mempty
    }
  , MkEncounterCard
    { ecCardCode = "01163"
    , ecName = "Rotting Remains"
    , ecCardType = TreacheryType
    , ecTraits = [Terror]
    , ecKeywords = mempty
    }
  , MkEncounterCard
    { ecCardCode = "01164"
    , ecName = "Frozen in Fear"
    , ecCardType = TreacheryType
    , ecTraits = [Terror]
    , ecKeywords = mempty
    }
  , MkEncounterCard
    { ecCardCode = "01165"
    , ecName = "Dissonant Voices"
    , ecCardType = TreacheryType
    , ecTraits = [Terror]
    , ecKeywords = mempty
    }
  , MkEncounterCard
    { ecCardCode = "01166"
    , ecName = "Ancient Evils"
    , ecCardType = TreacheryType
    , ecTraits = [Omen]
    , ecKeywords = mempty
    }
  , MkEncounterCard
    { ecCardCode = "01167"
    , ecName = "Crypt Chill"
    , ecCardType = TreacheryType
    , ecTraits = [Hazard]
    , ecKeywords = mempty
    }
  ]
