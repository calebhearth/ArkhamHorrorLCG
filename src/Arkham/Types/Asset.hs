{-# LANGUAGE FlexibleContexts #-}
module Arkham.Types.Asset
  ( lookupAsset, Asset )
where

import           Arkham.Types.AssetId
import           Arkham.Types.Card
import           Arkham.Types.Classes
import           Arkham.Types.InvestigatorId
import           Arkham.Types.LocationId
import           Arkham.Types.Message
import           Arkham.Types.Trait
import           ClassyPrelude
import           Data.Aeson
-- import           Data.Coerce
import qualified Data.HashMap.Strict         as HashMap
import qualified Data.HashSet                as HashSet
import           Lens.Micro
import           Safe                        (fromJustNote)

lookupAsset :: CardCode -> (AssetId -> Asset)
lookupAsset = fromJustNote "Unkown asset" . flip HashMap.lookup allAssets

allAssets :: HashMap CardCode (AssetId -> Asset)
allAssets = HashMap.fromList
  [ ("01020", machete)
  , ("01021", guardDog)
  ]

data Slot = HandSlot
    | AllySlot
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data AssetOwner = Location LocationId
    | Investigator InvestigatorId
    | Unowned
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Attrs = Attrs
    { assetName         :: Text
    , assetId           :: AssetId
    , assetCardCode     :: CardCode
    , assetCost         :: Int
    , assetOwner        :: AssetOwner
    , assetActions      :: [Message]
    , assetSlots        :: [Slot]
    , assetHealth       :: Maybe Int
    , assetSanity       :: Maybe Int
    , assetHealthDamage :: Int
    , assetSanityDamage :: Int
    , assetTraits       :: HashSet Trait
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Asset = Machete MacheteI
    | GuardDog GuardDogI
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- assetAttrs :: Asset -> Attrs
-- assetAttrs = \case
--   GuardDog attrs -> coerce attrs
--   Machete attrs -> coerce attrs

baseAttrs :: AssetId -> CardCode -> Int -> Text -> HashSet Trait -> Attrs
baseAttrs eid cardCode cost name traits = Attrs
  { assetName = name
  , assetId = eid
  , assetCardCode = cardCode
  , assetCost = cost
  , assetOwner = Unowned
  , assetActions = mempty
  , assetSlots = mempty
  , assetHealth = Nothing
  , assetSanity = Nothing
  , assetHealthDamage = 0
  , assetSanityDamage = 0
  , assetTraits = traits
  }

newtype MacheteI = MacheteI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

machete :: AssetId -> Asset
machete uuid = Machete $ MacheteI $ (baseAttrs uuid "01020" 3 "Machete" traits) { assetSlots = [HandSlot] }
  where traits = HashSet.fromList [Item, Weapon, Melee]

newtype GuardDogI = GuardDogI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

guardDog :: AssetId -> Asset
guardDog uuid = GuardDog $ GuardDogI $ (baseAttrs uuid "01021" 3 "Guard Dog" traits) { assetSlots = [AllySlot], assetHealth = Just 3, assetSanity = Just 1 }
  where traits = HashSet.fromList [Ally, Creature]

instance (HasQueue env) => RunMessage env Asset where
  runMessage msg = \case
    GuardDog x -> GuardDog <$> runMessage msg x
    Machete x -> Machete <$> runMessage msg x

instance (HasQueue env) => RunMessage env GuardDogI where
  runMessage msg (GuardDogI attrs@Attrs{..}) = case msg of
    AssetDamage aid eid _ _ | aid == assetId -> do
      unshiftMessage
        (Ask $ ChooseTo
          (EnemyDamaged
            eid
            (AssetSource aid)
            1
          )
        )
      GuardDogI <$> runMessage msg attrs
    _ -> GuardDogI <$> runMessage msg attrs

instance (HasQueue env) => RunMessage env MacheteI where
  runMessage msg (MacheteI attrs) = MacheteI <$> runMessage msg attrs

instance (HasQueue env) => RunMessage env Attrs where
  runMessage _ = pure
