{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator (hasEndedTurn, lookupInvestigator, GetInvestigatorId(..), Investigator) where

import           Arkham.Types.ActId
import           Arkham.Types.AssetId
import           Arkham.Types.Card
import           Arkham.Types.Classes
import           Arkham.Types.EnemyId
import           Arkham.Types.Helpers
import           Arkham.Types.InvestigatorId
import           Arkham.Types.LocationId
import           Arkham.Types.Message
import           Arkham.Types.SkillType
import           Arkham.Types.Token
import           ClassyPrelude
import           Data.Aeson
import           Data.Coerce
import qualified Data.HashMap.Strict         as HashMap
import qualified Data.HashSet                as HashSet
import           Lens.Micro
import           Lens.Micro.Extras
import           Safe                        (fromJustNote)
import           System.Random.Shuffle

lookupInvestigator :: InvestigatorId -> Investigator
lookupInvestigator iid = fromJustNote ("Unkown investigator: " <> show iid) $ HashMap.lookup iid allInvestigators

allInvestigators :: HashMap InvestigatorId Investigator
allInvestigators = HashMap.fromList $ map (\s -> (investigatorId . investigatorAttrs $ s, s)) [rolandBanks, daisyWalker]

instance HasCardCode Investigator where
  getCardCode = getCardCode . investigatorAttrs

instance HasCardCode Attrs where
  getCardCode = unInvestigatorId . investigatorId

data Attrs = Attrs
    { investigatorName               :: Text
    , investigatorId                 :: InvestigatorId
    , investigatorHealth             :: Int
    , investigatorSanity             :: Int
    , investigatorWillpower          :: Int
    , investigatorIntellect          :: Int
    , investigatorCombat             :: Int
    , investigatorAgility            :: Int
    , investigatorHealthDamage       :: Int
    , investigatorSanityDamage       :: Int
    , investigatorClues              :: Int
    , investigatorResources          :: Int
    , investigatorLocation           :: LocationId
    , investigatorRemainingActions   :: Int
    , investigatorEndedTurn          :: Bool
    , investigatorEngagedEnemies     :: HashSet EnemyId
    , investigatorAssets             :: HashSet AssetId
    , investigatorDeck               :: [PlayerCard]
    , investigatorDiscard            :: [PlayerCard]
    , investigatorHand               :: [Card]
    , investigatorConnectedLocations :: HashSet LocationId
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance HasSet AssetId () Investigator where
  getSet _ = investigatorAssets . investigatorAttrs

instance HasLocation Investigator where
  locationOf = investigatorLocation . investigatorAttrs

instance HasClueCount Investigator where
  getClueCount = ClueCount . investigatorClues . investigatorAttrs

class GetInvestigatorId a where
  getInvestigatorId :: a -> InvestigatorId

instance GetInvestigatorId Investigator where
  getInvestigatorId = investigatorId . investigatorAttrs

locationId :: Lens' Attrs LocationId
locationId = lens investigatorLocation $ \m x -> m { investigatorLocation = x }

connectedLocations :: Lens' Attrs (HashSet LocationId)
connectedLocations = lens investigatorConnectedLocations $ \m x -> m { investigatorConnectedLocations = x }

endedTurn :: Lens' Attrs Bool
endedTurn = lens investigatorEndedTurn $ \m x -> m { investigatorEndedTurn = x }

resources :: Lens' Attrs Int
resources = lens investigatorResources $ \m x -> m { investigatorResources = x }

clues :: Lens' Attrs Int
clues = lens investigatorClues $ \m x -> m { investigatorClues = x }

remainingActions :: Lens' Attrs Int
remainingActions = lens investigatorRemainingActions $ \m x -> m { investigatorRemainingActions = x }

engagedEnemies :: Lens' Attrs (HashSet EnemyId)
engagedEnemies = lens investigatorEngagedEnemies $ \m x -> m { investigatorEngagedEnemies = x }

assets :: Lens' Attrs (HashSet AssetId)
assets = lens investigatorAssets $ \m x -> m { investigatorAssets = x }

healthDamage :: Lens' Attrs Int
healthDamage = lens investigatorHealthDamage $ \m x -> m { investigatorHealthDamage = x }

sanityDamage :: Lens' Attrs Int
sanityDamage = lens investigatorSanityDamage $ \m x -> m { investigatorSanityDamage = x }

discard :: Lens' Attrs [PlayerCard]
discard = lens investigatorDeck $ \m x -> m { investigatorDiscard = x }

hand :: Lens' Attrs [Card]
hand = lens investigatorHand $ \m x -> m { investigatorHand = x }

deck :: Lens' Attrs [PlayerCard]
deck = lens investigatorDeck $ \m x -> m { investigatorDeck = x }

drawCard :: [PlayerCard] -> (Maybe PlayerCard, [PlayerCard])
drawCard []     = (Nothing, [])
drawCard (x:xs) = (Just x, xs)

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
  , investigatorRemainingActions = 3
  , investigatorEndedTurn = False
  , investigatorEngagedEnemies = mempty
  , investigatorAssets = mempty
  , investigatorDeck =
    map lookupCard ["01020", "01020", "01021", "01021"]
  -- , investigatorDeck = mempty
  , investigatorDiscard = mempty
  , investigatorHand = [PlayerCard $ lookupCard "01088", PlayerCard $ lookupCard "01021", PlayerCard $ lookupCard "01020"]
  -- , investigatorHand = mempty
  , investigatorConnectedLocations = mempty
  }

newtype RolandBanksI = RolandBanksI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

hasEndedTurn :: Investigator -> Bool
hasEndedTurn = view endedTurn . investigatorAttrs

rolandBanks :: Investigator
rolandBanks = RolandBanks $ RolandBanksI $ baseAttrs "01001" "Roland Banks" 9 5 3 2 4 2

newtype DaisyWalkerI = DaisyWalkerI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

daisyWalker :: Investigator
daisyWalker = DaisyWalker $ DaisyWalkerI $ baseAttrs "01002" "Daisy Walker" 5 9 3 5 2 2

type InvestigatorRunner env =
  ( HasCount ClueCount LocationId env
  , HasSet DamageableAssetId InvestigatorId env
  , HasQueue env
  , HasSet AdvanceableActId () env
  , HasSet ConnectedLocationId LocationId env
  )

instance (InvestigatorRunner env) => RunMessage env Investigator where
  runMessage msg = \case
    RolandBanks x -> RolandBanks <$> runMessage msg x
    DaisyWalker x -> DaisyWalker <$> runMessage msg x

sourceIsInvestigator :: Source -> Attrs -> Bool
sourceIsInvestigator source Attrs {..} = case source of
  InvestigatorSource sourceId -> sourceId == investigatorId
  AssetSource sourceId        -> sourceId `elem` investigatorAssets
  _                           -> False

instance (InvestigatorRunner env) => RunMessage env RolandBanksI where
  runMessage msg rb@(RolandBanksI attrs@Attrs {..}) = case msg of
    EnemyDefeated _ source | sourceIsInvestigator source attrs ->
      RolandBanksI <$> runMessage msg attrs <* unshiftMessage
        (Ask $ ChooseTo
          (ActivateCardAbilityAction
            investigatorId
            (getCardCode attrs)
            1
          )
        )
    ActivateCardAbilityAction _ cardCode n
      | cardCode == getCardCode attrs -> case n of
        1 -> rb <$ unshiftMessage (DiscoverClueAtLocation investigatorId investigatorLocation)
        _ -> pure rb
    ResolveToken ElderSign iid skillValue | iid == investigatorId -> do
      clueCount <- unClueCount <$> asks (getCount investigatorLocation)
      rb <$ runCheck (skillValue + clueCount)
    _ -> RolandBanksI <$> runMessage msg attrs

instance (InvestigatorRunner env) => RunMessage env DaisyWalkerI where
  runMessage msg (DaisyWalkerI attrs) = DaisyWalkerI <$> runMessage msg attrs

lookupCard :: CardCode -> PlayerCard
lookupCard cardCode = fromJustNote "Unknown card" $ HashMap.lookup cardCode allPlayerCards

instance (InvestigatorRunner env) => RunMessage env Attrs where
  runMessage msg a@Attrs {..} = case msg of
    EnemySpawn lid eid | lid == investigatorLocation ->
      a <$ unshiftMessage (EnemyEngageInvestigator eid investigatorId)
    EnemyEngageInvestigator eid iid | iid == investigatorId -> pure $ a & engagedEnemies %~ HashSet.insert eid
    EnemyDefeated eid _ -> pure $ a & engagedEnemies %~ HashSet.delete eid
    AssetDiscarded aid cardCode | aid `elem` investigatorAssets ->
      pure $ a & assets %~ HashSet.delete aid & discard %~ (lookupCard cardCode:)
    ChooseMoveAction iid | iid == investigatorId ->
      a <$ unshiftMessage (Ask $ ChooseOne $ map (\l -> ChoiceResults [CheckAttackOfOpportunity iid, MoveTo iid l]) (HashSet.toList investigatorConnectedLocations))
    InvestigatorAssignDamage iid eid health sanity | iid == investigatorId -> do
      allDamageableAssets <- HashSet.toList . HashSet.map unDamageableAssetId <$> asks (getSet iid)
      a <$ unshiftMessage
        (Ask $ ChooseOne
          (ChoiceResult (InvestigatorDamage investigatorId (EnemySource eid) health sanity)
          : map (\k -> ChoiceResult $ AssetDamage k eid health sanity) allDamageableAssets
          )
        )
    ChooseInvestigateAction iid | iid == investigatorId -> do
      unshiftMessages [CheckAttackOfOpportunity iid, Investigate SkillIntellect investigatorIntellect iid investigatorLocation]
      pure $ a & remainingActions -~ 1
    DiscoverClue iid | iid == investigatorId -> pure $ a & clues +~ 1
    PayCardCost iid _ n | iid == investigatorId -> do
      let cost = getCost $ a ^?! hand . ix n
      pure $ a & resources -~ cost
    InvestigatorPlayCard iid _ n | iid == investigatorId -> pure $ a & hand %~ without n
    InvestigatorPlayAsset iid aid | iid == investigatorId -> pure $ a & assets %~ HashSet.insert aid
    InvestigatorDamage iid _ health sanity
      | iid == investigatorId -> pure $ a & healthDamage +~ health & sanityDamage +~ sanity
    MoveAllTo lid -> a <$ unshiftMessage (MoveTo investigatorId lid)
    MoveTo iid lid | iid == investigatorId -> do
      connectedLocations' <- HashSet.map unConnectedLocationId <$> asks (getSet lid)
      pure $ a  & locationId .~ lid & connectedLocations .~ connectedLocations'
    AddedConnection lid1 lid2 | lid1 == investigatorLocation || lid2 == investigatorLocation->
      pure $ a & connectedLocations %~ HashSet.insert lid1 & connectedLocations %~ HashSet.insert lid2
    ChooseEndTurn iid | iid == investigatorId -> pure $ a & endedTurn .~ True
    BeginRound -> pure $ a & endedTurn .~ False & remainingActions .~ 3
    ChooseDrawCardAction iid | iid == investigatorId -> do
      unshiftMessages [CheckAttackOfOpportunity iid, DrawCards iid 1]
      pure $ a & remainingActions -~ 1
    DrawCards iid _ | iid == investigatorId -> do
      let (mcard, deck') = drawCard investigatorDeck
          handUpdate = maybe id ((:) . PlayerCard) mcard
      when (null deck') $ unshiftMessage (EmptyDeck investigatorId)
      pure $ a & hand %~ handUpdate
    ChooseTakeResourceAction iid | iid == investigatorId -> do
      unshiftMessages [CheckAttackOfOpportunity iid, TakeResources iid 1]
      pure $ a & remainingActions -~ 1
    ChoosePlayCardAction iid | iid == investigatorId -> do
      unshiftMessage (Ask $ ChooseOne $ zipWith (\i c -> ChoiceResults [PayCardCost iid (getCardCode c) (i -1), CheckAttackOfOpportunity iid, InvestigatorPlayCard iid (getCardCode c) (i - 1)]) [1..] investigatorHand)
      pure $ a & remainingActions -~ 1
    TakeResources iid n | iid == investigatorId -> pure $ a & resources +~ n
    EmptyDeck iid | iid == investigatorId -> do
      deck' <- liftIO $ shuffleM investigatorDiscard
      pure $ a & discard .~ [] & deck .~ deck'
    AllDrawCardAndResource -> do
      let (mcard, deck') = drawCard investigatorDeck
          handUpdate = maybe id ((:) . PlayerCard) mcard
      when (null deck') $ unshiftMessage (EmptyDeck investigatorId)
      pure $ a & resources +~ 1 & hand %~ handUpdate & deck .~ deck'
    PlayerWindow iid | iid == investigatorId -> do
      advanceableActIds <- HashSet.toList . HashSet.map unAdvanceableActId <$> asks (getSet ())
      let advanceActions = map (ChoiceResult . AdvanceAct) advanceableActIds
      if a ^. remainingActions > 0
         then do
           let enemyActions =
                 if HashSet.size investigatorEngagedEnemies == 0
                    then []
                    else [ ChooseFightEnemyAction
                         , ChooseEngageEnemyAction
                         , ChooseEvadeEnemyAction
                         ]
               moveActions = [ ChooseMoveAction | HashSet.size investigatorConnectedLocations > 0]

           a <$ unshiftMessage ( Ask
             $ ChooseOne
             $ map (ChoiceResult . ($ iid))
               ([ ChooseTakeResourceAction
                 , ChooseDrawCardAction
                 , ChoosePlayCardAction -- TODO: Can you actually play a card?
                 , ChooseActivateCardAbilityAction -- TODO: Can you actually trigger an ability
                 ]
                 <> moveActions
                 <> [ ChooseInvestigateAction ]
                 <> enemyActions
                 <> [ ChooseEndTurn ]
               ) <> advanceActions
            )
        else a <$ unshiftMessage (Ask $ ChooseOne $ advanceActions <> [ChoiceResult $ ChooseEndTurn iid])
    _ -> pure a
