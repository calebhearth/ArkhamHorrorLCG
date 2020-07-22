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
  )
where

import           Arkham.Types.SkillType
import           Arkham.Types.Trait
import           ClassyPrelude
import           Data.Aeson
import qualified Data.HashMap.Strict    as HashMap

class HasCardCode a where
  getCardCode :: a -> CardCode

class HasCost a where
  getCost :: a -> Int

newtype CardCode = CardCode { unCardCode :: Text }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, IsString)

data PlayerCardType = AssetType
    | EventType
    | SkillType
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data EncounterCardType = TreacheryType
    | EnemyType
    | LocationType
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data ClassSymbol = Guardian
    | Seeker
    | Survivor
    | Rogue
    | Mystic
    | Neutral
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Card = PlayerCard PlayerCard
    | EncounterCard EncounterCard
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance HasCardCode Card where
  getCardCode (PlayerCard    card) = getCardCode card
  getCardCode (EncounterCard card) = getCardCode card

instance HasCost Card where
  getCost (PlayerCard card) = getCost card
  getCost _                 = 0


data PlayerCard = MkPlayerCard
    { pcCardCode   :: CardCode
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
    { ecCardType :: EncounterCardType
    , ecCardCode :: CardCode
    , ecTraits   :: [Trait]
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance HasCardCode EncounterCard where
  getCardCode = ecCardCode

allCards :: HashMap CardCode Card
allCards = HashMap.map PlayerCard allPlayerCards

allPlayerCards :: HashMap CardCode PlayerCard
allPlayerCards = HashMap.fromList $ map
  (\c -> (getCardCode c, c))
  [ MkPlayerCard "01020"
                 3
                 0
                 AssetType
                 Guardian
                 [SkillCombat]
                 [Item, Weapon, Melee]
  , MkPlayerCard "01021" 3 0 AssetType Guardian [SkillCombat] [Ally, Creature]
  , MkPlayerCard "01088" 0 0 EventType Neutral  []            [Supply]
  ]
