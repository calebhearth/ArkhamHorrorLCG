{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Attrs where

import Arkham.Types.ActId
import Arkham.Types.Action (Action)
import qualified Arkham.Types.Action as Action
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.Helpers
import Arkham.Types.Investigator.Runner
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Stats
import Arkham.Types.Trait
import Arkham.Types.TreacheryId
import ClassyPrelude
import Data.Aeson
import qualified Data.HashSet as HashSet
import Lens.Micro
import Safe (fromJustNote)
import System.Random.Shuffle

instance HasCardCode Attrs where
  getCardCode = unInvestigatorId . investigatorId

data Attrs = Attrs
  { investigatorName :: Text
  , investigatorId :: InvestigatorId
  , investigatorHealth :: Int
  , investigatorSanity :: Int
  , investigatorWillpower :: Int
  , investigatorIntellect :: Int
  , investigatorCombat :: Int
  , investigatorAgility :: Int
  , investigatorHealthDamage :: Int
  , investigatorSanityDamage :: Int
  , investigatorClues :: Int
  , investigatorResources :: Int
  , investigatorLocation :: LocationId
  , investigatorActionsTaken :: [Action]
  , investigatorRemainingActions :: Int
  , investigatorEndedTurn :: Bool
  , investigatorEngagedEnemies :: HashSet EnemyId
  , investigatorAssets :: HashSet AssetId
  , investigatorDeck :: [PlayerCard]
  , investigatorDiscard :: [PlayerCard]
  , investigatorHand :: [Card]
  , investigatorConnectedLocations :: HashSet LocationId
  , investigatorTraits :: HashSet Trait
  , investigatorTreacheries :: HashSet TreacheryId
  , investigatorModifiers :: [Modifier]
  , investigatorAbilities :: [(Source, Int)]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

locationId :: Lens' Attrs LocationId
locationId = lens investigatorLocation $ \m x -> m { investigatorLocation = x }

modifiers :: Lens' Attrs [Modifier]
modifiers =
  lens investigatorModifiers $ \m x -> m { investigatorModifiers = x }

connectedLocations :: Lens' Attrs (HashSet LocationId)
connectedLocations = lens investigatorConnectedLocations
  $ \m x -> m { investigatorConnectedLocations = x }

endedTurn :: Lens' Attrs Bool
endedTurn =
  lens investigatorEndedTurn $ \m x -> m { investigatorEndedTurn = x }

resources :: Lens' Attrs Int
resources =
  lens investigatorResources $ \m x -> m { investigatorResources = x }

clues :: Lens' Attrs Int
clues = lens investigatorClues $ \m x -> m { investigatorClues = x }

remainingActions :: Lens' Attrs Int
remainingActions = lens investigatorRemainingActions
  $ \m x -> m { investigatorRemainingActions = x }

actionsTaken :: Lens' Attrs [Action]
actionsTaken =
  lens investigatorActionsTaken $ \m x -> m { investigatorActionsTaken = x }

engagedEnemies :: Lens' Attrs (HashSet EnemyId)
engagedEnemies =
  lens investigatorEngagedEnemies $ \m x -> m { investigatorEngagedEnemies = x }

assets :: Lens' Attrs (HashSet AssetId)
assets = lens investigatorAssets $ \m x -> m { investigatorAssets = x }

treacheries :: Lens' Attrs (HashSet TreacheryId)
treacheries =
  lens investigatorTreacheries $ \m x -> m { investigatorTreacheries = x }

healthDamage :: Lens' Attrs Int
healthDamage =
  lens investigatorHealthDamage $ \m x -> m { investigatorHealthDamage = x }

sanityDamage :: Lens' Attrs Int
sanityDamage =
  lens investigatorSanityDamage $ \m x -> m { investigatorSanityDamage = x }

discard :: Lens' Attrs [PlayerCard]
discard = lens investigatorDiscard $ \m x -> m { investigatorDiscard = x }

hand :: Lens' Attrs [Card]
hand = lens investigatorHand $ \m x -> m { investigatorHand = x }

deck :: Lens' Attrs [PlayerCard]
deck = lens investigatorDeck $ \m x -> m { investigatorDeck = x }

skillValueFor :: SkillType -> Attrs -> Int
skillValueFor skill attrs = foldr
  applyModifier
  baseSkillValue
  (investigatorModifiers attrs)
 where
  applyModifier (SkillModifier skillType m _) n =
    if skillType == skill then n + m else n
  applyModifier _ n = n
  baseSkillValue = case skill of
    SkillWillpower -> investigatorWillpower attrs
    SkillIntellect -> investigatorIntellect attrs
    SkillCombat -> investigatorCombat attrs
    SkillAgility -> investigatorAgility attrs
    SkillWild -> error "investigators do not have wild skills"

baseAttrs :: InvestigatorId -> Text -> Stats -> [Trait] -> Attrs
baseAttrs iid name Stats {..} traits = Attrs
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
  , investigatorClues = 20
  , investigatorResources = 5
  , investigatorLocation = "00000"
  , investigatorActionsTaken = mempty
  , investigatorRemainingActions = 3
  , investigatorEndedTurn = False
  , investigatorEngagedEnemies = mempty
  , investigatorAssets = mempty
  , investigatorDeck = mempty
  , investigatorDiscard = mempty
  -- , investigatorHand = mempty
  , investigatorHand =
    [ PlayerCard $ lookupPlayerCard "01088"
    , PlayerCard $ lookupPlayerCard "01021"
    , PlayerCard $ lookupPlayerCard "01020"
    ]
  , investigatorConnectedLocations = mempty
  , investigatorTraits = HashSet.fromList traits
  , investigatorTreacheries = mempty
  , investigatorModifiers = mempty
  , investigatorAbilities = mempty
  }

sourceIsInvestigator :: Source -> Attrs -> Bool
sourceIsInvestigator source Attrs {..} = case source of
  InvestigatorSource sourceId -> sourceId == investigatorId
  AssetSource sourceId -> sourceId `elem` investigatorAssets
  _ -> False

matchTarget :: Attrs -> ActionTarget -> Action -> Bool
matchTarget attrs (FirstOneOf as) action =
  action `elem` as && action `notElem` investigatorActionsTaken attrs
matchTarget _ (IsAction a) action = action == a

actionCost :: Attrs -> Action -> Int
actionCost attrs a = foldr applyModifier 1 (investigatorModifiers attrs)
 where
  applyModifier (ActionCostOf match m _) n =
    if matchTarget attrs match a then n + m else n
  applyModifier _ n = n

canPerform :: Attrs -> Action -> Bool
canPerform a@Attrs {..} actionType =
  actionCost a actionType <= investigatorRemainingActions

isPlayable :: Attrs -> Card -> Bool
isPlayable _ (EncounterCard _) = False -- TODO: there might be some playable ones?
isPlayable Attrs {..} (PlayerCard MkPlayerCard {..}) =
  (pcCardType /= SkillType)
    && (pcCost <= investigatorResources)
    && none prevents investigatorModifiers
 where
  none f = not . any f
  prevents (CannotPlay types _) = pcCardType `elem` types
  prevents _ = False

takeAction :: Action -> Attrs -> Attrs
takeAction action a =
  a
    & (remainingActions -~ actionCost a action)
    & (actionsTaken %~ (<> [action]))

instance (InvestigatorRunner env) => RunMessage env Attrs where
  runMessage msg a@Attrs {..} = case msg of
    EnemySpawn lid eid | lid == investigatorLocation ->
      a <$ unshiftMessage (EnemyEngageInvestigator eid investigatorId)
    EnemyMove eid _ lid | lid == investigatorLocation ->
      a <$ unshiftMessage (EnemyEngageInvestigator eid investigatorId)
    EnemyEngageInvestigator eid iid | iid == investigatorId ->
      pure $ a & engagedEnemies %~ HashSet.insert eid
    EnemyDefeated eid _ _ -> pure $ a & engagedEnemies %~ HashSet.delete eid
    RemoveEnemy eid -> pure $ a & engagedEnemies %~ HashSet.delete eid
    ChooseAndDiscardAsset iid | iid == investigatorId -> a <$ unshiftMessage
      (Ask $ ChooseOne $ map
        (ChoiceResult . DiscardAsset)
        (HashSet.toList $ a ^. assets)
      )
    AttachTreacheryToInvestigator tid iid | iid == investigatorId ->
      pure $ a & treacheries %~ HashSet.insert tid
    DiscardTreachery tid | tid `elem` investigatorTreacheries ->
      pure $ a & treacheries %~ HashSet.delete tid
    AssetDiscarded aid cardCode | aid `elem` investigatorAssets ->
      pure
        $ a
        & (assets %~ HashSet.delete aid)
        & (discard %~ (lookupPlayerCard cardCode :))
    ChooseFightEnemyAction iid | iid == investigatorId -> a <$ unshiftMessage
      (Ask $ ChooseOne $ map
        (\eid -> ChoiceResults
          [ WhenAttackEnemy iid eid
          , AttackEnemy iid eid SkillCombat (skillValueFor SkillCombat a) 1
          , AfterAttackEnemy iid eid
          ]
        )
        (HashSet.toList investigatorEngagedEnemies)
      )
    ChooseMoveAction iid | iid == investigatorId -> do
      blockedLocationIds <- HashSet.map unBlockedLocationId <$> asks (getSet ())
      let
        accessibleLocations =
          investigatorConnectedLocations `difference` blockedLocationIds
      a <$ unshiftMessage
        (Ask $ ChooseOne $ map
          (\l -> ChoiceResults [CheckAttackOfOpportunity iid, MoveAction iid l])
          (HashSet.toList accessibleLocations)
        )
    MoveAction iid l | iid == investigatorId -> do
      unshiftMessage (MoveTo iid l)
      pure $ takeAction Action.Move a
    InvestigatorAssignDamage iid eid health sanity | iid == investigatorId -> do
      allDamageableAssets <-
        HashSet.toList . HashSet.map unDamageableAssetId <$> asks (getSet iid)
      a <$ unshiftMessage
        (Ask $ ChooseOne
          (ChoiceResult
              (InvestigatorDamage investigatorId (EnemySource eid) health sanity
              )
          : map
              (\k -> ChoiceResult $ AssetDamage k eid health sanity)
              allDamageableAssets
          )
        )
    ChooseInvestigateAction iid | iid == investigatorId -> do
      unshiftMessages
        [ CheckAttackOfOpportunity iid
        , Investigate
          SkillIntellect
          investigatorIntellect
          iid
          investigatorLocation
        ]
      pure $ takeAction Action.Investigate a
    DiscoverClue iid | iid == investigatorId -> pure $ a & clues +~ 1
    PayCardCost iid _ n | iid == investigatorId -> do
      let cost = getCost $ a ^?! hand . ix n
      pure $ a & resources -~ cost
    InvestigatorPlayCard iid _ n | iid == investigatorId ->
      pure $ a & hand %~ without n
    InvestigatorPlayAsset iid aid | iid == investigatorId ->
      pure $ a & assets %~ HashSet.insert aid
    InvestigatorDamage iid _ health sanity | iid == investigatorId ->
      pure $ a & healthDamage +~ health & sanityDamage +~ sanity
    MoveAllTo lid -> a <$ unshiftMessage (MoveTo investigatorId lid)
    MoveTo iid lid | iid == investigatorId -> do
      connectedLocations' <- HashSet.map unConnectedLocationId
        <$> asks (getSet lid)
      unshiftMessages [WhenEnterLocation iid lid, AfterEnterLocation iid lid]
      pure $ a & locationId .~ lid & connectedLocations .~ connectedLocations'
    AddedConnection lid1 lid2
      | lid1 == investigatorLocation || lid2 == investigatorLocation
      -> pure
        $ a
        & (connectedLocations %~ HashSet.insert lid1)
        & (connectedLocations %~ HashSet.insert lid2)
    InvestigatorAddModifier iid modifier | iid == investigatorId ->
      pure $ a & modifiers %~ (modifier :)
    InvestigatorRemoveAllModifiersFromSource iid source
      | iid == investigatorId -> pure $ a & modifiers %~ filter
        ((source /=) . sourceOfModifier)
    ChooseEndTurn iid | iid == investigatorId -> pure $ a & endedTurn .~ True
    BeginRound ->
      pure
        $ a
        & (endedTurn .~ False)
        & (remainingActions .~ 3)
        & (actionsTaken .~ mempty)
    ChooseDrawCardAction iid | iid == investigatorId -> do
      unshiftMessages [CheckAttackOfOpportunity iid, DrawCards iid 1]
      pure $ takeAction Action.Draw a
    DrawCards iid _ | iid == investigatorId -> do
      let
        (mcard, deck') = drawCard investigatorDeck
        handUpdate = maybe id ((:) . PlayerCard) mcard
      when (null deck') $ unshiftMessage (EmptyDeck investigatorId)
      pure $ a & hand %~ handUpdate
    ChooseTakeResourceAction iid | iid == investigatorId -> do
      unshiftMessages [CheckAttackOfOpportunity iid, TakeResources iid 1]
      pure $ takeAction Action.Resource a
    ChoosePlayCardAction iid | iid == investigatorId -> do
      unshiftMessage
        (Ask $ ChooseOne $ zipWith
          (\i c -> ChoiceResults
            [ PayCardCost iid (getCardCode c) (i - 1)
            , CheckAttackOfOpportunity iid
            , InvestigatorPlayCard iid (getCardCode c) (i - 1)
            ]
          )
          [1 ..]
          (filter (isPlayable a) investigatorHand)
        )
      pure $ takeAction Action.Play a
    TakeResources iid n | iid == investigatorId -> pure $ a & resources +~ n
    EmptyDeck iid | iid == investigatorId -> do
      deck' <- liftIO $ shuffleM investigatorDiscard
      pure $ a & discard .~ [] & deck .~ deck'
    AllDrawEncounterCard ->
      a <$ unshiftMessage (InvestigatorDrawEncounterCard investigatorId)
    RevelationSkillCheck iid skillType difficulty onSuccess onFailure ->
      a <$ unshiftMessage
        (BeginSkillCheck
          iid
          skillType
          difficulty
          (skillValueFor skillType a)
          onSuccess
          onFailure
        )
    ActivateCardAbilityAction iid (InvestigatorSource sourceId) n
      | sourceId == investigatorId
      -> let
           (source, idx) =
             fromJustNote "no ability" $ investigatorAbilities !!? n
         in a <$ unshiftMessage (UseCardAbility iid source idx)
    AllDrawCardAndResource -> do
      let
        (mcard, deck') = drawCard investigatorDeck
        handUpdate = maybe id ((:) . PlayerCard) mcard
      when (null deck') $ unshiftMessage (EmptyDeck investigatorId)
      pure $ a & resources +~ 1 & hand %~ handUpdate & deck .~ deck'
    PlayerWindow iid | iid == investigatorId -> do
      advanceableActIds <-
        HashSet.toList . HashSet.map unAdvanceableActId <$> asks (getSet ())
      let advanceActions = map (ChoiceResult . AdvanceAct) advanceableActIds
      if a ^. remainingActions > 0
        then do
          blockedLocationIds <- HashSet.map unBlockedLocationId
            <$> asks (getSet ())
          let
            accessibleLocations =
              investigatorConnectedLocations `difference` blockedLocationIds
            playableCards = filter (isPlayable a) investigatorHand
            enemyActions = if HashSet.size investigatorEngagedEnemies == 0
              then []
              else map snd $ filter
                (canPerform a . fst)
                [ (Action.Fight, ChooseFightEnemyAction)
                , (Action.Engage, ChooseEngageEnemyAction)
                , (Action.Evade, ChooseEvadeEnemyAction)
                ]
            moveActions =
              [ ChooseMoveAction
              | (HashSet.size accessibleLocations > 0)
                && canPerform a Action.Move
              ]
            playActions =
              [ ChoosePlayCardAction
              | not (null playableCards) && canPerform a Action.Play
              ]
            investigateActions =
              [ ChooseInvestigateAction | canPerform a Action.Investigate ]

          a <$ unshiftMessage
            (Ask
            $ ChooseOne
            $ map
                (ChoiceResult . ($ iid))
                (map
                    snd
                    (filter
                      (canPerform a . fst)
                      [ (Action.Resource, ChooseTakeResourceAction)
                      , (Action.Draw, ChooseDrawCardAction)
                      , (Action.Ability, ChooseActivateCardAbilityAction)
                      ]
                    )
                <> playActions
                <> moveActions
                <> investigateActions
                <> enemyActions
                <> [ChooseEndTurn]
                )
            <> advanceActions
            )
        else a <$ unshiftMessage
          (Ask
          $ ChooseOne
          $ advanceActions
          <> [ChoiceResult $ ChooseEndTurn iid]
          )
    _ -> pure a
