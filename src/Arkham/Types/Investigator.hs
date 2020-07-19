module Arkham.Types.Investigator (hasEndedTurn, lookupInvestigator, GetInvestigatorId(..), Investigator) where

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
import           Lens.Micro.Extras
import           Safe                        (fromJustNote)

lookupInvestigator :: InvestigatorId -> Investigator
lookupInvestigator iid = fromJustNote ("Unkown investigator: " <> show iid) $ HashMap.lookup iid allInvestigators

allInvestigators :: HashMap InvestigatorId Investigator
allInvestigators = HashMap.fromList $ map (\s -> (investigatorId . investigatorAttrs $ s, s)) [rolandBanks, daisyWalker]

data Attrs = Attrs
    { investigatorName             :: Text
    , investigatorId               :: InvestigatorId
    , investigatorHealth           :: Int
    , investigatorSanity           :: Int
    , investigatorWillpower        :: Int
    , investigatorIntellect        :: Int
    , investigatorCombat           :: Int
    , investigatorAgility          :: Int
    , investigatorHealthDamage     :: Int
    , investigatorSanityDamage     :: Int
    , investigatorClues            :: Int
    , investigatorResources        :: Int
    , investigatorLocation         :: LocationId
    , investigatorActionsRemaining :: Int
    , investigatorEndedTurn        :: Bool
    , investigatorEngagedEnemies   :: HashSet EnemyId
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance HasLocation Investigator where
  locationOf = investigatorLocation . investigatorAttrs

class GetInvestigatorId a where
  getInvestigatorId :: a -> InvestigatorId

instance GetInvestigatorId Investigator where
  getInvestigatorId = investigatorId . investigatorAttrs

locationId :: Lens' Attrs LocationId
locationId = lens investigatorLocation $ \m x -> m { investigatorLocation = x }

endedTurn :: Lens' Attrs Bool
endedTurn = lens investigatorEndedTurn $ \m x -> m { investigatorEndedTurn = x }

resources :: Lens' Attrs Int
resources = lens investigatorResources $ \m x -> m { investigatorResources = x }

engagedEnemies :: Lens' Attrs (HashSet EnemyId)
engagedEnemies = lens investigatorEngagedEnemies $ \m x -> m { investigatorEngagedEnemies = x }

healthDamage :: Lens' Attrs Int
healthDamage = lens investigatorHealthDamage $ \m x -> m { investigatorHealthDamage = x }

sanityDamage :: Lens' Attrs Int
sanityDamage = lens investigatorSanityDamage $ \m x -> m { investigatorSanityDamage = x }

data Investigator = RolandBanks RolandBanksI
    | DaisyWalker DaisyWalkerI
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

investigatorAttrs :: Investigator -> Attrs
investigatorAttrs = \case
  RolandBanks attrs -> coerce attrs
  DaisyWalker attrs -> coerce attrs

baseAttrs :: InvestigatorId -> Text -> Int -> Int -> Int -> Int -> Int -> Int -> Attrs
baseAttrs iid name health sanity willpower intellect combat agility = Attrs
  { investigatorName = name
  , investigatorId = iid
  , investigatorHealth = health
  , investigatorSanity = sanity
  , investigatorWillpower = willpower
  , investigatorIntellect = intellect
  , investigatorCombat = combat
  , investigatorAgility = agility
  , investigatorHealthDamage = 0
  , investigatorSanityDamage = 0
  , investigatorClues = 0
  , investigatorResources = 5
  , investigatorLocation = "00000"
  , investigatorActionsRemaining = 3
  , investigatorEndedTurn = False
  , investigatorEngagedEnemies = mempty
  }

newtype RolandBanksI = RolandBanksI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

hasEndedTurn :: Investigator -> Bool
hasEndedTurn = view endedTurn . investigatorAttrs

rolandBanks :: Investigator
rolandBanks = RolandBanks $ RolandBanksI $ baseAttrs "01001" "Roland Banks" 9 5 3 3 4 2

newtype DaisyWalkerI = DaisyWalkerI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

daisyWalker :: Investigator
daisyWalker = DaisyWalker $ DaisyWalkerI $ baseAttrs "01002" "Daisy Walker" 5 9 3 5 2 2

instance (HasQueue env) => RunMessage env Investigator where
  runMessage msg = \case
    RolandBanks x -> RolandBanks <$> runMessage msg x
    DaisyWalker x -> DaisyWalker <$> runMessage msg x

instance (HasQueue env) => RunMessage env RolandBanksI where
  runMessage msg (RolandBanksI attrs) = RolandBanksI <$> runMessage msg attrs

instance (HasQueue env) => RunMessage env DaisyWalkerI where
  runMessage msg (DaisyWalkerI attrs) = DaisyWalkerI <$> runMessage msg attrs

instance (HasQueue env) => RunMessage env Attrs where
  runMessage msg a@Attrs {..} = case msg of
    EnemySpawn lid eid | lid == investigatorLocation ->
      a <$ unshiftMessage (EnemyEngageInvestigator eid investigatorId)
    EnemyEngageInvestigator eid iid | iid == investigatorId -> pure $ a & engagedEnemies %~ HashSet.insert eid
    InvestigatorAssignDamage iid _ health sanity | iid == investigatorId -> pure $ a & healthDamage +~ health & sanityDamage +~ sanity
      -- let
      --   damagableAssets =
      --     map (\a -> ChoiceResult $ AssetDamaged (_assetId a) eid amount)
      --       $ filter (isJust . _assetHealth)
      --       $ map toAsset _investigatorAssets
      -- i <$ unshiftMessage
      --   (Ask $ ChooseOne
      --     (ChoiceResult (InvestigatorDamage investigatorId' eid amount)
      --     : damagableAssets
      --     )
      --   )
    MoveAllTo lid -> a <$ unshiftMessage (MoveTo investigatorId lid)
    MoveTo iid lid | iid == investigatorId -> pure $ a  & locationId .~ lid
    ChooseEndTurn iid | iid == investigatorId -> pure $ a & endedTurn .~ True
    ChooseTakeResourceAction iid | iid == investigatorId ->
      a <$ traverse unshiftMessage (reverse [CheckAttackOfOpportunity iid, TakeResources iid 1])
    TakeResources iid n | iid == investigatorId -> pure $ a & resources +~ n
    PlayerWindow iid | iid == investigatorId -> a <$ unshiftMessage
      ( Ask
      $ ChooseOne
        [ ChoiceResult $ ChooseTakeResourceAction iid
        , ChoiceResult $ ChooseDrawCardAction iid
        , ChoiceResult $ ChoosePlayCardAction iid
        , ChoiceResult $ ChooseActiveCardAbilityAction iid
        , ChoiceResult $ ChooseMoveAction iid
        , ChoiceResult $ ChooseInvestigateAction iid
        , ChoiceResult $ ChooseFightEnemyAction iid
        , ChoiceResult $ ChooseEngageEnemyAction iid
        , ChoiceResult $ ChooseEvadeEnemyAction iid
        , ChoiceResult $ ChooseEndTurn iid
        ]
      )
    _ -> pure a
