module Arkham.Types.SkillCheck
  ( SkillCheck(..)
  , DrawStrategy(..)
  , ResolveStrategy(..)
  )
where

import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
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

data SkillCheck = SkillCheck
  { skillCheckInvestigator    :: InvestigatorId
  , skillCheckDifficulty      :: Int
  , skillCheckOnSuccess       :: [Message]
  , skillCheckOnFailure       :: [Message]
  , skillCheckDrawStrategy    :: DrawStrategy
  , skillCheckResolveStrategy :: ResolveStrategy
  , skillCheckSetAsideTokens  :: [Token]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

setAsideTokens :: Lens' SkillCheck [Token]
setAsideTokens =
  lens skillCheckSetAsideTokens $ \m x -> m { skillCheckSetAsideTokens = x }

onFailure :: Lens' SkillCheck [Message]
onFailure = lens skillCheckOnFailure $ \m x -> m { skillCheckOnFailure = x }

instance (HasQueue env) => RunMessage env SkillCheck where
  runMessage msg s@SkillCheck {..} = case msg of
    AddOnFailure m -> pure $ s & onFailure %~ (m :)
    DrawToken token -> pure $ s & setAsideTokens %~ (token :)
    FailSkillCheck -> do
      unshiftMessage SkillTestEnds
      s <$ unshiftMessages skillCheckOnFailure
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
        then s <$ unshiftMessages skillCheckOnSuccess
        else s <$ unshiftMessages skillCheckOnFailure
    _ -> pure s
