{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.RolandBanks where

import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Source
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype RolandBanksI = RolandBanksI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

rolandBanks :: RolandBanksI
rolandBanks = RolandBanksI $ baseAttrs
  "01001"
  "Roland Banks"
  Stats
    { health = 9
    , sanity = 5
    , willpower = 3
    , intellect = 3
    , combat = 4
    , agility = 2
    }
  [Agency, Detective]

instance (InvestigatorRunner env) => RunMessage env RolandBanksI where
  runMessage msg rb@(RolandBanksI attrs@Attrs {..}) = case msg of
    EnemyDefeated _ _ source | sourceIsInvestigator source attrs ->
      RolandBanksI <$> runMessage msg attrs <* unshiftMessage
        (Ask
        $ ChooseTo
            (UseCardAbility
              investigatorId
              (InvestigatorSource investigatorId)
              1
            )
        )
    UseCardAbility _ (InvestigatorSource iid) 1 | iid == investigatorId ->
      rb <$ unshiftMessage
        (DiscoverClueAtLocation investigatorId investigatorLocation)
    ResolveToken ElderSign iid skillValue | iid == investigatorId -> do
      clueCount <- unClueCount <$> asks (getCount investigatorLocation)
      rb <$ runCheck (skillValue + clueCount)
    _ -> RolandBanksI <$> runMessage msg attrs
