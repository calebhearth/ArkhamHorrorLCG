module Arkham.Types.SkillCheck
  ( SkillCheck(..)
  , DrawStrategy(..)
  , ResolveStrategy(..)
  )
where

import           Arkham.Types.Classes
import           Arkham.Types.InvestigatorId
import           Arkham.Types.Message
import           Arkham.Types.Token
import           ClassyPrelude
import           Data.Aeson
import           Lens.Micro

data DrawStrategy = DrawOne
    | DrawX Int
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data ResolveStrategy = ResolveAll
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

onFailure :: Lens' SkillCheck [Message]
onFailure = lens skillCheckOnFailure $ \m x -> m { skillCheckOnFailure = x }

instance (HasQueue env) => RunMessage env SkillCheck where
  runMessage msg s@SkillCheck {..} = case msg of
    AddOnFailure m -> pure $ s & onFailure %~ (m :)
    FailSkillCheck ->
      s <$ traverse_ unshiftMessage (reverse skillCheckOnFailure)
    SkillTestEnds ->
      s <$ unshiftMessage (ReturnTokens skillCheckSetAsideTokens)
    RunSkillCheck modifiedSkillValue -> do
      unshiftMessage SkillTestEnds
      putStrLn
        .  pack
        $  "Modified skill value: "
        <> show modifiedSkillValue
        <> "\nDifficulty: "
        <> show skillCheckDifficulty
      if modifiedSkillValue >= skillCheckDifficulty
        then s <$ traverse_ unshiftMessage (reverse skillCheckOnSuccess)
        else s <$ traverse_ unshiftMessage (reverse skillCheckOnFailure)
    _ -> pure s
