{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Agenda
  ( Agenda(..)
  , lookupAgenda
  )
where

import           Arkham.Types.AgendaId
import           Arkham.Types.Classes
import           Arkham.Types.GameValue
import           Arkham.Types.Message
import           ClassyPrelude
import           Data.Aeson
import           Data.Coerce
import qualified Data.HashMap.Strict    as HashMap
import           Lens.Micro
import           Safe                   (fromJustNote)

lookupAgenda :: AgendaId -> Agenda
lookupAgenda = fromJustNote "Unknown agenda" . flip HashMap.lookup allAgendas

allAgendas :: HashMap AgendaId Agenda
allAgendas = HashMap.fromList $ map
  (\a -> (agendaId $ agendaAttrs a, a))
  [whatsGoingOn, riseOfTheGhouls, theyreGettingOut]

data Attrs = Attrs
    { agendaDoom          :: Int
    , agendaDoomThreshold :: GameValue
    , agendaId            :: AgendaId
    , agendaName          :: Text
    , agendaSequence      :: Text
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

doom :: Lens' Attrs Int
doom = lens agendaDoom $ \m x -> m { agendaDoom = x }

doomThreshold :: Lens' Attrs GameValue
doomThreshold = lens agendaDoomThreshold $ \m x -> m { agendaDoomThreshold = x }

baseAttrs :: AgendaId -> Text -> Text -> GameValue -> Attrs
baseAttrs aid name seq' threshold = Attrs
  { agendaDoom     = 0
  , agendaDoomThreshold = threshold
  , agendaId       = aid
  , agendaName     = name
  , agendaSequence = seq'
  }

data Agenda = WhatsGoingOn WhatsGoingOnI
    | RiseOfTheGhouls RiseOfTheGhoulsI
    | TheyreGettingOut TheyreGettingOutI
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

agendaAttrs :: Agenda -> Attrs
agendaAttrs = \case
  WhatsGoingOn attrs -> coerce attrs
  RiseOfTheGhouls attrs -> coerce attrs
  TheyreGettingOut attrs -> coerce attrs

newtype WhatsGoingOnI = WhatsGoingOnI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

whatsGoingOn :: Agenda
whatsGoingOn = WhatsGoingOn . WhatsGoingOnI $ baseAttrs "01105" "What's Going On?!" "Agenda 1a" (Static 3)
newtype RiseOfTheGhoulsI = RiseOfTheGhoulsI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

riseOfTheGhouls :: Agenda
riseOfTheGhouls = RiseOfTheGhouls . RiseOfTheGhoulsI $ baseAttrs "01106" "Rise of the Ghouls" "Agenda 2a" (Static 7)

newtype TheyreGettingOutI = TheyreGettingOutI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

theyreGettingOut :: Agenda
theyreGettingOut = TheyreGettingOut . TheyreGettingOutI $ baseAttrs "01107" "They're Getting Out!" "Agenda 3a" (Static 10)

type AgendaRunner env = (HasId LeadInvestigatorId env, HasCount PlayerCount () env, HasQueue env)

instance (AgendaRunner env) => RunMessage env Agenda where
  runMessage msg = \case
    WhatsGoingOn x -> WhatsGoingOn <$> runMessage msg x
    RiseOfTheGhouls x -> RiseOfTheGhouls <$> runMessage msg x
    TheyreGettingOut x -> TheyreGettingOut <$> runMessage msg x

instance (AgendaRunner env) => RunMessage env WhatsGoingOnI where
  runMessage msg a@(WhatsGoingOnI attrs@Attrs {..}) =
    case msg of
      AdvanceAgenda aid | aid == agendaId -> do
        leadInvestigatorId <- unLeadInvestigatorId <$> asks getId
        a <$ unshiftMessages
          [ Ask $ ChooseOne [ChoiceResult AllRandomDiscard, ChoiceResult (InvestigatorDamage leadInvestigatorId (AgendaSource aid) 0 2)]
          , NextAgenda aid "01106"
          ]
      _ -> WhatsGoingOnI <$> runMessage msg attrs

instance (AgendaRunner env) => RunMessage env RiseOfTheGhoulsI where
  runMessage msg (RiseOfTheGhoulsI attrs) = RiseOfTheGhoulsI <$> runMessage msg attrs

instance (AgendaRunner env) => RunMessage env TheyreGettingOutI where
  runMessage msg (TheyreGettingOutI attrs) = TheyreGettingOutI <$> runMessage msg attrs

instance (AgendaRunner env) => RunMessage env Attrs where
  runMessage msg a@Attrs {..} = case msg of
    PlaceDoomOnAgenda -> pure $ a & doom +~ 1
    AdvanceAgendaIfThresholdSatisfied -> do
      pc <- unPlayerCount <$> asks (getCount ())
      when (a ^. doom + 1 > fromGameValue (a ^. doomThreshold) pc) (unshiftMessage (AdvanceAgenda agendaId))
      pure a
    _ -> pure a
