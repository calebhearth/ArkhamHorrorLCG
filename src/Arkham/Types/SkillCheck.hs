module Arkham.Types.SkillCheck
  ( SkillCheck(..)
  , DrawStrategy(..)
  , ResolveStrategy(..)
  , SkillCheckResult(..)
  )
where

import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Token
import ClassyPrelude
import Data.Aeson
import Lens.Micro

data DrawStrategy
  = DrawOne
  | DrawX Int
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ResolveStrategy
  = ResolveAll
  | ResolveOne
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SkillCheckResult = Unrun | SucceededBy Int | FailedBy Int
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SkillCheck = SkillCheck
  { skillCheckInvestigator    :: InvestigatorId
  , skillCheckSkillType :: SkillType
  , skillCheckDifficulty      :: Int
  , skillCheckOnSuccess       :: [Message]
  , skillCheckOnFailure       :: [Message]
  , skillCheckDrawStrategy    :: DrawStrategy
  , skillCheckResolveStrategy :: ResolveStrategy
  , skillCheckSetAsideTokens  :: [Token]
  , skillCheckResult :: SkillCheckResult
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

setAsideTokens :: Lens' SkillCheck [Token]
setAsideTokens =
  lens skillCheckSetAsideTokens $ \m x -> m { skillCheckSetAsideTokens = x }

result :: Lens' SkillCheck SkillCheckResult
result = lens skillCheckResult $ \m x -> m { skillCheckResult = x }

onFailure :: Lens' SkillCheck [Message]
onFailure = lens skillCheckOnFailure $ \m x -> m { skillCheckOnFailure = x }

instance (HasQueue env) => RunMessage env SkillCheck where
  runMessage msg s@SkillCheck {..} = case msg of
    AddOnFailure m -> pure $ s & onFailure %~ (m :)
    HorrorPerPointOfFailure iid -> case skillCheckResult of
      FailedBy n ->
        s <$ unshiftMessage (InvestigatorDamage iid SkillCheckSource 0 n)
      _ -> error "Should not be called when not failed"
    DamagePerPointOfFailure iid -> case skillCheckResult of
      FailedBy n ->
        s <$ unshiftMessage (InvestigatorDamage iid SkillCheckSource n 0)
      _ -> error "Should not be called when not failed"
    DrawToken token -> pure $ s & setAsideTokens %~ (token :)
    FailSkillCheck -> do
      unshiftMessage SkillTestEnds
      unshiftMessages skillCheckOnFailure
      pure $ s & result .~ FailedBy skillCheckDifficulty
    SkillTestEnds ->
      s <$ unshiftMessage (ReturnTokens skillCheckSetAsideTokens)
    RunSkillCheck modifiedSkillValue -> do
      unshiftMessage SkillTestEnds
      putStrLn
        . pack
        $ "Modified skill value: "
        <> show modifiedSkillValue
        <> "\nDifficulty: "
        <> show skillCheckDifficulty
      if modifiedSkillValue >= skillCheckDifficulty
        then do
          unshiftMessages skillCheckOnSuccess
          pure $ s & result .~ SucceededBy
            (modifiedSkillValue - skillCheckDifficulty)
        else do
          unshiftMessages skillCheckOnFailure
          pure $ s & result .~ FailedBy
            (skillCheckDifficulty - modifiedSkillValue)
    _ -> pure s
