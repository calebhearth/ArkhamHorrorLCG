{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery
  ( lookupTreachery
  , Treachery(..)
  )
where

import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Trait
import Arkham.Types.TreacheryId
import ClassyPrelude
import Data.Aeson
import Data.Coerce
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Safe (fromJustNote)

lookupTreachery :: CardCode -> (TreacheryId -> Treachery)
lookupTreachery =
  fromJustNote "Unkown treachery" . flip HashMap.lookup allTreacheries

allTreacheries :: HashMap CardCode (TreacheryId -> Treachery)
allTreacheries = HashMap.fromList
  [ ("01162", graspingHands)
  , ("01166", ancientEvils)
  , ("01163", rottingRemains)
  , ("01167", cryptChill)
  ]

instance HasCardCode Treachery where
  getCardCode = treacheryCardCode . treacheryAttrs

instance HasTraits Treachery where
  traitsOf = treacheryTraits . treacheryAttrs

data Attrs = Attrs
  { treacheryName :: Text
  , treacheryId :: TreacheryId
  , treacheryCardCode :: CardCode
  , treacheryTraits :: HashSet Trait
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Treachery
  = GraspingHands GraspingHandsI
  | AncientEvils AncientEvilsI
  | RottingRemains RottingRemainsI
  | CryptChill CryptChillI
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

treacheryAttrs :: Treachery -> Attrs
treacheryAttrs = \case
  GraspingHands attrs -> coerce attrs
  AncientEvils attrs -> coerce attrs
  RottingRemains attrs -> coerce attrs
  CryptChill attrs -> coerce attrs

baseAttrs :: TreacheryId -> CardCode -> Attrs
baseAttrs tid cardCode =
  let
    MkEncounterCard {..} =
      fromJustNote "missing encounter card"
        $ HashMap.lookup cardCode allEncounterCards
  in
    Attrs
      { treacheryName = ecName
      , treacheryId = tid
      , treacheryCardCode = ecCardCode
      , treacheryTraits = HashSet.fromList ecTraits
      }

newtype GraspingHandsI = GraspingHandsI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

graspingHands :: TreacheryId -> Treachery
graspingHands uuid = GraspingHands $ GraspingHandsI $ baseAttrs uuid "01162"

newtype AncientEvilsI = AncientEvilsI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

ancientEvils :: TreacheryId -> Treachery
ancientEvils uuid = AncientEvils $ AncientEvilsI $ baseAttrs uuid "01166"

newtype RottingRemainsI = RottingRemainsI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

rottingRemains :: TreacheryId -> Treachery
rottingRemains uuid = RottingRemains $ RottingRemainsI $ baseAttrs uuid "01163"

newtype CryptChillI = CryptChillI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

cryptChill :: TreacheryId -> Treachery
cryptChill uuid = CryptChill $ CryptChillI $ baseAttrs uuid "01167"

type TreacheryRunner env = (HasQueue env, HasSet AssetId InvestigatorId env)

instance (TreacheryRunner env) => RunMessage env Treachery where
  runMessage msg = \case
    GraspingHands x -> GraspingHands <$> runMessage msg x
    AncientEvils x -> AncientEvils <$> runMessage msg x
    RottingRemains x -> RottingRemains <$> runMessage msg x
    CryptChill x -> CryptChill <$> runMessage msg x

instance (TreacheryRunner env) => RunMessage env GraspingHandsI where
  runMessage msg t@(GraspingHandsI attrs@Attrs {..}) = case msg of
    RunTreachery iid tid | tid == treacheryId -> t <$ unshiftMessage
      (RevelationSkillCheck iid SkillAgility 3 [] [DamagePerPointOfFailure iid])
    _ -> GraspingHandsI <$> runMessage msg attrs

instance (TreacheryRunner env) => RunMessage env AncientEvilsI where
  runMessage msg t@(AncientEvilsI attrs@Attrs {..}) = case msg of
    RunTreachery _ tid | tid == treacheryId ->
      t <$ unshiftMessages
        [PlaceDoomOnAgenda, AdvanceAgendaIfThresholdSatisfied]
    _ -> AncientEvilsI <$> runMessage msg attrs

instance (TreacheryRunner env) => RunMessage env RottingRemainsI where
  runMessage msg t@(RottingRemainsI attrs@Attrs {..}) = case msg of
    RunTreachery iid tid | tid == treacheryId -> t <$ unshiftMessage
      (RevelationSkillCheck
        iid
        SkillWillpower
        3
        []
        [HorrorPerPointOfFailure iid]
      )
    _ -> RottingRemainsI <$> runMessage msg attrs

instance (TreacheryRunner env) => RunMessage env CryptChillI where
  runMessage msg t@(CryptChillI attrs@Attrs {..}) = case msg of
    RunTreachery iid tid | tid == treacheryId -> t <$ unshiftMessage
      (RevelationSkillCheck iid SkillWillpower 4 [] [TreacheryFailure iid tid])
    TreacheryFailure iid tid | tid == treacheryId -> do
      assetCount <- HashSet.size <$> asks (getSet @AssetId iid)
      if assetCount > 0
        then t <$ unshiftMessage (ChooseAndDiscardAsset iid)
        else
          t <$ unshiftMessage
            (InvestigatorDamage iid (TreacherySource treacheryId) 2 0)
    _ -> CryptChillI <$> runMessage msg attrs

instance (TreacheryRunner env) => RunMessage env Attrs where
  runMessage _msg a@Attrs {..} = pure a
