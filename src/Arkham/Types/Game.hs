{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module Arkham.Types.Game
  ( runGame
  , newGame
  , Game(..)
  )
where

import           Arkham.Types.Asset
import           Arkham.Types.AssetId
import           Arkham.Types.Card
import           Arkham.Types.Classes
import           Arkham.Types.Difficulty
import           Arkham.Types.Enemy
import           Arkham.Types.EnemyId
import           Arkham.Types.Event
import           Arkham.Types.GameJson
import           Arkham.Types.Helpers
import           Arkham.Types.Investigator
import           Arkham.Types.InvestigatorId
import           Arkham.Types.Location
import           Arkham.Types.LocationId
import           Arkham.Types.Message
import           Arkham.Types.Phase
import           Arkham.Types.Scenario
import           Arkham.Types.ScenarioId
import           Arkham.Types.SkillCheck
import           Arkham.Types.Token          (Token)
import qualified Arkham.Types.Token          as Token
import           Arkham.Types.Trait
import           ClassyPrelude
import qualified Data.HashMap.Strict         as HashMap
import qualified Data.HashSet                as HashSet
import           Data.UUID.V4
import           Lens.Micro
import           Lens.Micro.Extras
import           Lens.Micro.Platform         ()
import           Safe                        (fromJustNote)
import           System.Random
import           Text.Pretty.Simple
import           Text.Read

data Game = Game
    { giMessages             :: IORef [Message]
    , giScenario             :: Scenario
    , giLocations            :: HashMap LocationId Location
    , giInvestigators        :: HashMap InvestigatorId Investigator
    , giEnemies              :: HashMap EnemyId Enemy
    , giAssets               :: HashMap AssetId Asset
    , giActiveInvestigatorId :: InvestigatorId
    , giLeadInvestigatorId   :: InvestigatorId
    , giPhase                :: Phase
    , giDiscard              :: [CardCode]
    , giChaosBag             :: [Token]
    , giSkillCheck           :: Maybe SkillCheck
    }

locations :: Lens' Game (HashMap LocationId Location)
locations = lens giLocations $ \m x -> m { giLocations = x }

investigators :: Lens' Game (HashMap InvestigatorId Investigator)
investigators = lens giInvestigators $ \m x -> m { giInvestigators = x }

enemies :: Lens' Game (HashMap EnemyId Enemy)
enemies = lens giEnemies $ \m x -> m { giEnemies = x }

assets :: Lens' Game (HashMap AssetId Asset)
assets = lens giAssets $ \m x -> m { giAssets = x }

discard :: Lens' Game [CardCode]
discard = lens giDiscard $ \m x -> m { giDiscard = x }

chaosBag :: Lens' Game [Token]
chaosBag = lens giChaosBag $ \m x -> m { giChaosBag = x }

activeInvestigatorId :: Lens' Game InvestigatorId
activeInvestigatorId =
  lens giActiveInvestigatorId $ \m x -> m { giActiveInvestigatorId = x }

scenario :: Lens' Game Scenario
scenario = lens giScenario $ \m x -> m { giScenario = x }

skillCheck :: Lens' Game (Maybe SkillCheck)
skillCheck = lens giSkillCheck $ \m x -> m { giSkillCheck = x }

activeInvestigator :: Game -> Investigator
activeInvestigator g =
  fromJustNote "No active investigator" $ g ^? investigators . ix iid
  where iid = g ^. activeInvestigatorId

newGame :: MonadIO m => ScenarioId -> [Investigator] -> m Game
newGame scenarioId investigatorsList = do
  ref <- newIORef [Setup]
  pure $ Game { giMessages             = ref
              , giScenario             = lookupScenario scenarioId Easy
              , giLocations            = mempty
              , giEnemies            = mempty
              , giAssets            = mempty
              , giInvestigators        = investigatorsMap
              , giActiveInvestigatorId = initialInvestigatorId
              , giLeadInvestigatorId = initialInvestigatorId
              , giPhase                = Investigation
              , giDiscard = mempty
              , giSkillCheck = Nothing
              , giChaosBag =
                [ Token.PlusOne
                , Token.PlusOne
                , Token.Zero
                , Token.Zero
                , Token.Zero
                , Token.MinusOne
                , Token.MinusOne
                , Token.MinusOne
                , Token.MinusTwo
                , Token.MinusTwo
                , Token.Skull
                , Token.Skull
                , Token.Cultist
                , Token.Tablet
                , Token.AutoFail
                , Token.ElderSign
                ]
              }
 where
  initialInvestigatorId =
    fromJustNote "No investigators" . headMay . HashMap.keys $ investigatorsMap
  investigatorsMap =
    HashMap.fromList $ map (\i -> (getInvestigatorId i, i)) investigatorsList

instance HasCount ClueCount LocationId Game where
  getCount lid g = fromJustNote "No location" $ getClueCount <$> g ^? locations . ix lid

instance HasCount PlayerCount () Game where
  getCount _ = PlayerCount . HashMap.size . view investigators


-- fromSet :: (Eq key) => HashSet key -> HashMap key value -> [value]
-- fromSet hset hmap = HashMap.foldrWithKey (\k v vs -> if k `elem` hset then (v:vs) else vs) [] hmap

instance HasCount EnemyCount (LocationId, [Trait]) Game where
  getCount (lid, traits) g@Game {..} = EnemyCount . length $ HashSet.filter enemyMatcher locationEnemies
    where location = fromJustNote "No location" $ g ^? locations . ix lid
          locationEnemies = getSet location
          enemyMatcher eid =  all (flip HashSet.member (traitsOf $ g ^?! enemies . ix eid)) traits

instance HasCount EnemyCount (CurrentInvestigatorLocation, [Trait]) Game where
  getCount (_, traits) g@Game {..}  = getCount (locationId, traits) g
    where locationId = locationFor giActiveInvestigatorId g

instance HasSet DamageableAssetId Game where
  getSet = HashSet.map DamageableAssetId . HashMap.keysSet . HashMap.filter isDamageable . view assets

instance HasQueue Game where
  messageQueue = lens giMessages $ \m x -> m { giMessages = x }

createEnemy :: MonadIO m => CardCode -> m (EnemyId, Enemy)
createEnemy cardCode = do
  eid <- liftIO $ EnemyId <$> nextRandom
  pure (eid, lookupEnemy cardCode eid)

locationFor :: InvestigatorId -> Game -> LocationId
locationFor iid g = locationOf investigator
 where
  investigator = fromJustNote "could not find investigator" $ g ^? investigators . ix iid

drawToken :: MonadIO m => Game -> m (Token, [Token])
drawToken Game {..} = do
  n <- liftIO $ randomRIO (0, length giChaosBag - 1)
  let token = fromJustNote "impossivle" $ giChaosBag !!? n
  pPrint token
  pure (token, without n giChaosBag)

runGameMessage :: (HasQueue env, MonadReader env m, MonadIO m) => Message -> Game -> m Game
runGameMessage msg g = case msg of
  PlaceLocation lid -> pure $ g & locations . at lid ?~ lookupLocation lid
  ReturnTokens tokens -> pure $ g & chaosBag %~ (tokens <>)
  InvestigatorPlayCard iid cardCode _ -> do
    let card = fromJustNote "Could not find card" $ HashMap.lookup cardCode allCards
    case card of
      PlayerCard pc ->
        case pcCardType pc of
          AssetType -> do
            let builder = fromJustNote "could not find asset" $ HashMap.lookup cardCode allAssets
            aid <- liftIO $ AssetId <$> nextRandom
            unshiftMessage (InvestigatorPlayAsset iid aid)
            pure $ g & assets %~ HashMap.insert aid (builder aid)
          EventType -> do
            let eventMessages = ($ iid) . fromJustNote "could not find event" $ HashMap.lookup cardCode allEvents
            traverse_ unshiftMessage (reverse eventMessages)
            pure g
          _ -> pure g
      EncounterCard _ -> pure g
  EnemyWillAttack iid eid -> do
    mNextMessage <- peekMessage
    case mNextMessage of
      Just (EnemyAttacks as) -> do
        _ <- popMessage
        unshiftMessage (EnemyAttacks (EnemyAttack iid eid : as))
      Just aoo@(CheckAttackOfOpportunity _) -> do
        _ <- popMessage
        unshiftMessage msg
        unshiftMessage aoo
      Just (EnemyWillAttack iid2 eid2) -> do
        _ <- popMessage
        unshiftMessage
          (EnemyAttacks [EnemyAttack iid eid, EnemyAttack iid2 eid2])
      _ -> unshiftMessage (EnemyAttack iid eid)
    pure g
  EnemyAttacks as -> do
    mNextMessage <- peekMessage
    case mNextMessage of
      Just (EnemyAttacks as2) -> do
        _ <- popMessage
        unshiftMessage (EnemyAttacks $ as ++ as2)
      Just aoo@(CheckAttackOfOpportunity _) -> do
        _ <- popMessage
        unshiftMessage msg
        unshiftMessage aoo
      Just (EnemyWillAttack iid2 eid2) -> do
        _ <- popMessage
        unshiftMessage (EnemyAttacks (EnemyAttack iid2 eid2 : as))
      _ -> unshiftMessage (Ask . ChooseOneAtATime $ map ChoiceResult as)
    pure g
  AssetDefeated aid -> do
    let asset = g ^?! assets . ix aid
    unshiftMessage (AssetDiscarded aid (getCardCode asset))
    pure $ g & assets %~ HashMap.delete aid
  EnemyDefeated eid _ ->
    let enemy = g ^?! enemies . ix eid
    in
      pure
      $ g
      & (enemies %~ HashMap.delete eid)
      & (discard %~ (getCardCode enemy :))
  -- BeginInvestigation -> do
  --   let
  --     iid = fromJustNote "No investigators?" . headMay $ HashMap.keys (g ^. investigators)
  --   g <$ traverse_
  --     pushMessage
  --     [ InvestigatorPlayCard iid "01021"
  --     , InvestigatorDrawEncounterCard iid "01159"
  --     , InvestigatorDrawEncounterCard iid "01159"
  --     , InvestigatorDrawEncounterCard iid "01159"
  --     ]
  BeginSkillCheck iid _ difficulty skillValue onSuccess onFailure -> do
    (token, chaosBag') <- drawToken g
    unshiftMessage (ResolveToken token iid skillValue)
    pure $ g & skillCheck ?~ SkillCheck iid difficulty onSuccess onFailure DrawOne ResolveAll [token] & chaosBag .~ chaosBag'
  ResolveToken Token.PlusOne _ skillValue ->
    g <$ runCheck (skillValue + 1)
  ResolveToken Token.Zero _ skillValue ->
    g <$ runCheck skillValue
  ResolveToken Token.MinusOne _ skillValue ->
    g <$ runCheck (skillValue - 1)
  ResolveToken Token.MinusTwo _ skillValue ->
    g <$ runCheck (skillValue - 2)
  ResolveToken Token.MinusThree _ skillValue ->
    g <$ runCheck (skillValue - 3)
  ResolveToken Token.MinusFour _ skillValue ->
    g <$ runCheck (skillValue - 4)
  ResolveToken Token.MinusFive _ skillValue ->
    g <$ runCheck (skillValue - 5)
  ResolveToken Token.MinusSix _ skillValue ->
    g <$ runCheck (skillValue - 6)
  ResolveToken Token.MinusSeven _ skillValue ->
    g <$ runCheck (skillValue - 7)
  ResolveToken Token.MinusEight _ skillValue ->
    g <$ runCheck (skillValue - 8)
  ResolveToken Token.AutoFail _ _ -> g <$ unshiftMessage FailSkillCheck
  InvestigatorDrawEncounterCard iid cardCode -> do
    (enemyId', enemy') <- createEnemy cardCode
    let lid = locationFor iid g
    unshiftMessage (InvestigatorDrawEnemy iid lid enemyId')
    pure $ g & enemies . at enemyId' ?~ enemy'
  _                 -> pure g

instance RunMessage Game Game where
  runMessage msg g =
    traverseOf scenario (runMessage msg) g
      >>= traverseOf (locations . traverse)     (runMessage msg)
      >>= traverseOf (enemies . traverse) (runMessage msg)
      >>= traverseOf (assets . traverse) (runMessage msg)
      >>= traverseOf (investigators . traverse) (runMessage msg)
      >>= traverseOf (skillCheck . traverse) (runMessage msg)
      >>= runGameMessage msg

toExternalGame :: MonadIO m => Game -> m GameJson
toExternalGame Game {..} = do
  queue <- liftIO $ readIORef giMessages
  pure $ GameJson { gMessages      = queue
                  , gScenario      = giScenario
                  , gLocations     = giLocations
                  , gInvestigators = giInvestigators
                  , gEnemies = giEnemies
                  , gAssets = giAssets
                  , gActiveInvestigatorId = giActiveInvestigatorId
                  , gLeadInvestigatorId = giLeadInvestigatorId
                  , gPhase         = giPhase
                  , gDiscard         = giDiscard
                  , gSkillCheck = giSkillCheck
                  , gChaosBag         = giChaosBag
                  }

toInternalGame' :: IORef [Message] -> GameJson -> Game
toInternalGame' ref GameJson {..} = do
  Game { giMessages      = ref
       , giScenario      = gScenario
       , giLocations     = gLocations
       , giInvestigators = gInvestigators
       , giEnemies = gEnemies
       , giAssets = gAssets
       , giActiveInvestigatorId = gActiveInvestigatorId
       , giLeadInvestigatorId = gLeadInvestigatorId
       , giPhase         = gPhase
       , giDiscard         = gDiscard
       , giSkillCheck = gSkillCheck
       , giChaosBag = gChaosBag
       }

runMessages :: MonadIO m => Game -> m (Maybe Question, GameJson)
runMessages g = flip runReaderT g $ do
  liftIO $ readIORef (giMessages g) >>= pPrint
  mmsg <- popMessage
  case mmsg of
    Nothing -> case giPhase g of
      Resolution    -> (Nothing, ) <$> toExternalGame g
      Mythos        -> (Nothing, ) <$> toExternalGame g
      Enemy         -> (Nothing, ) <$> toExternalGame g
      Upkeep        -> (Nothing, ) <$> toExternalGame g
      Investigation -> if hasEndedTurn (activeInvestigator g)
        then (Nothing, ) <$> toExternalGame g
        else pushMessage (PlayerWindow $ g ^. activeInvestigatorId) >> runMessages g
    Just msg -> case msg of
      Ask q -> (Just q, ) <$> toExternalGame g
      _     -> runMessage msg g >>= runMessages

keepAsking :: forall a m . (Show a, Read a, MonadIO m) => String -> m a
keepAsking s = do
  putStr $ pack s
  liftIO $ hFlush stdout
  mresult <- readMaybe @a . unpack <$> getLine
  case mresult of
    Nothing -> keepAsking s
    Just a  -> pure a

extract :: Int -> [a] -> (Maybe a, [a])
extract n xs =
  let a = xs !!? (n - 1) in (a, [ x | (i, x) <- zip [1 ..] xs, i /= n ])

handleQuestion :: MonadIO m => GameJson -> Question -> m [Message]
handleQuestion _ = \case
  ChoiceResult msg -> pure [msg]
  q@(ChooseTo msg) -> do
    putStr $ pack $ show q
    liftIO $ hFlush stdout
    resp <- getLine
    if "n" `isPrefixOf` toLower resp then pure [] else pure [msg]
  ChooseOne [] -> pure []
  ChooseOne qs -> do
    i <- keepAsking @Int ("Choose one:\n\n" <> (unlines $ map show $ zip @_ @Int [1..] qs))
    pure $ maybeToList $ Ask <$> qs !!? (i - 1)
  ChooseOneAtATime [] -> pure []
  ChooseOneAtATime qs -> do
    i <- keepAsking @Int ("Choose one at a time:\n\n" <> (unlines $ map show $ zip @_ @Int [1..] qs))
    let (mq, qs') = extract i qs
    case mq of
      Just q' -> pure [Ask q', Ask $ ChooseOneAtATime qs']
      Nothing -> pure []

runGame :: MonadIO m => Game -> m GameJson
runGame g = do
  let ref = giMessages g
  (mQuestion, gameJson) <- runMessages g
  pPrint gameJson
  messages <- maybe (pure []) (handleQuestion gameJson) mQuestion
  modifyIORef' ref (\queue -> messages <> queue)
  messages' <- readIORef ref
  if null messages'
     then pure gameJson
     else runGame $ toInternalGame' ref gameJson
