{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset
  ( lookupAsset
  , allAssets
  , isDamageable
  , Asset
  )
where

import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson
import Data.Coerce
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Lens.Micro
import Safe (fromJustNote)

lookupAsset :: CardCode -> (AssetId -> Asset)
lookupAsset = fromJustNote "Unkown asset" . flip HashMap.lookup allAssets

allAssets :: HashMap CardCode (AssetId -> Asset)
allAssets = HashMap.fromList
  [("01020", machete), ("01021", guardDog), ("01117", litaChantler)]

instance HasCardCode Asset where
  getCardCode = assetCardCode . assetAttrs

instance HasId (Maybe OwnerId) () Asset where
  getId _ = (OwnerId <$>) . assetInvestigator . assetAttrs


data Slot
  = HandSlot
  | AllySlot
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Attrs = Attrs
  { assetName :: Text
  , assetId :: AssetId
  , assetCardCode :: CardCode
  , assetCost :: Int
  , assetInvestigator :: Maybe InvestigatorId
  , assetActions :: [Message]
  , assetSlots :: [Slot]
  , assetHealth :: Maybe Int
  , assetSanity :: Maybe Int
  , assetHealthDamage :: Int
  , assetSanityDamage :: Int
  , assetTraits :: HashSet Trait
  , assetAbilities :: [(Source, Int)]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

defeated :: Attrs -> Bool
defeated Attrs {..} =
  maybe False (assetHealthDamage >=) assetHealth
    || maybe False (assetSanityDamage >=) assetSanity

data Asset
  = Machete MacheteI
  | GuardDog GuardDogI
  | LitaChantler LitaChantlerI
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

assetAttrs :: Asset -> Attrs
assetAttrs = \case
  GuardDog attrs -> coerce attrs
  Machete attrs -> coerce attrs
  LitaChantler attrs -> coerce attrs

isDamageable :: Asset -> Bool
isDamageable a =
  (isJust . assetHealth . assetAttrs $ a)
    || (isJust . assetHealth . assetAttrs $ a)

baseAttrs :: AssetId -> CardCode -> Attrs
baseAttrs eid cardCode =
  let
    MkPlayerCard {..} = fromJustNote "missing player card"
      $ HashMap.lookup cardCode allPlayerCards
  in
    Attrs
      { assetName = pcName
      , assetId = eid
      , assetCardCode = cardCode
      , assetCost = pcCost
      , assetInvestigator = Nothing
      , assetActions = mempty
      , assetSlots = mempty
      , assetHealth = Nothing
      , assetSanity = Nothing
      , assetHealthDamage = 0
      , assetSanityDamage = 0
      , assetTraits = HashSet.fromList pcTraits
      , assetAbilities = mempty
      }

investigator :: Lens' Attrs (Maybe InvestigatorId)
investigator = lens assetInvestigator $ \m x -> m { assetInvestigator = x }

healthDamage :: Lens' Attrs Int
healthDamage = lens assetHealthDamage $ \m x -> m { assetHealthDamage = x }

sanityDamage :: Lens' Attrs Int
sanityDamage = lens assetSanityDamage $ \m x -> m { assetSanityDamage = x }

abilities :: Lens' Attrs [(Source, Int)]
abilities = lens assetAbilities $ \m x -> m { assetAbilities = x }

newtype MacheteI = MacheteI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

machete :: AssetId -> Asset
machete uuid =
  Machete $ MacheteI $ (baseAttrs uuid "01020") { assetSlots = [HandSlot] }

newtype GuardDogI = GuardDogI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

guardDog :: AssetId -> Asset
guardDog uuid = GuardDog $ GuardDogI $ (baseAttrs uuid "01021")
  { assetSlots = [AllySlot]
  , assetHealth = Just 3
  , assetSanity = Just 1
  }

newtype LitaChantlerI = LitaChantlerI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

litaChantler :: AssetId -> Asset
litaChantler uuid = LitaChantler $ LitaChantlerI $ (baseAttrs uuid "01117")
  { assetSlots = [AllySlot]
  , assetHealth = Just 3
  , assetSanity = Just 3
  }


type AssetRunner env
  = ( HasQueue env
    , HasSet InvestigatorId () env
    , HasSet InvestigatorId LocationId env
    , HasId LocationId InvestigatorId env
    )

instance (AssetRunner env) => RunMessage env Asset where
  runMessage msg = \case
    GuardDog x -> GuardDog <$> runMessage msg x
    Machete x -> Machete <$> runMessage msg x
    LitaChantler x -> LitaChantler <$> runMessage msg x

instance (AssetRunner env) => RunMessage env GuardDogI where
  runMessage msg (GuardDogI attrs@Attrs {..}) = case msg of
    AssetDamage aid eid _ _ | aid == assetId -> do
      -- we must unshift the asset destroyed first before unshifting the question
      -- this is necessary to keep the asset as a valid investigator source of damage
      -- for any additional effects, such as triggering Roland's ability.
      result <- runMessage msg attrs
      unshiftMessage (Ask $ ChooseTo (EnemyDamage eid (AssetSource aid) 1))
      pure $ GuardDogI result
    _ -> GuardDogI <$> runMessage msg attrs

instance (AssetRunner env) => RunMessage env MacheteI where
  runMessage msg (MacheteI attrs) = MacheteI <$> runMessage msg attrs

instance (AssetRunner env) => RunMessage env LitaChantlerI where
  runMessage msg a@(LitaChantlerI attrs@Attrs {..}) = case msg of
    WhenAttackEnemy iid eid -> case assetInvestigator of
      Just ownerId -> do
        locationId <- asks (getId @LocationId ownerId)
        locationInvestigatorIds <- HashSet.toList <$> asks (getSet locationId)
        if iid `elem` locationInvestigatorIds
          then a <$ unshiftMessage
            (Ask $ ChooseTo
              (EnemyAddModifier eid (DamageTaken 1 (AssetSource assetId)))
            )
          else pure a
      _ -> pure a
    AfterAttackEnemy _ eid -> a <$ unshiftMessage
      (EnemyRemoveAllModifiersFromSource eid (AssetSource assetId))
    PostPlayerWindow -> do
      allInvestigatorIds <- HashSet.toList <$> asks (getSet ())
      case assetInvestigator of
        Just ownerId -> do
          locationId <- asks (getId @LocationId ownerId)
          locationInvestigatorIds <- HashSet.toList <$> asks (getSet locationId)
          unshiftMessages $ map
            (\iid -> InvestigatorAddModifier
              iid
              (SkillModifier SkillCombat 1 (AssetSource assetId))
            )
            locationInvestigatorIds
        _ -> pure ()
      unshiftMessages $ map
        (\iid ->
          InvestigatorRemoveAllModifiersFromSource iid (AssetSource assetId)
        )
        allInvestigatorIds
      pure a
    _ -> LitaChantlerI <$> runMessage msg attrs

instance (AssetRunner env) => RunMessage env Attrs where
  runMessage msg a@Attrs {..} = case msg of
    AddAbility (AssetSource aid) ability | aid == assetId ->
      pure $ a & abilities %~ (<> [ability])
    RemoveAbilitiesFrom source -> do
      let
        abilities' = filter (\(source', _) -> source /= source') assetAbilities
      pure $ a & abilities .~ abilities'
    ActivateCardAbilityAction iid (AssetSource aid) n | aid == assetId ->
      let (source, idx) = fromJustNote "no ability" $ assetAbilities !!? n
      in a <$ unshiftMessage (UseCardAbility iid source idx)
    AssetDamage aid _ health sanity | aid == assetId -> do
      let a' = a & healthDamage +~ health & sanityDamage +~ sanity
      when (defeated a') (unshiftMessage (AssetDefeated aid))
      pure a'
    InvestigatorPlayAsset iid aid | aid == assetId ->
      pure $ a & investigator ?~ iid
    TakeControlOfAsset iid aid | aid == assetId ->
      pure $ a & investigator ?~ iid
    _ -> pure a
