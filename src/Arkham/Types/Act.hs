module Arkham.Types.Act
  ( Act(..)
  , lookupAct
  )
where

import           Arkham.Types.ActId
import           Arkham.Types.Classes
import           ClassyPrelude
import           Data.Aeson
import           Data.Coerce
import qualified Data.HashMap.Strict  as HashMap
import           Safe                 (fromJustNote)

lookupAct :: ActId -> Act
lookupAct = fromJustNote "Unknown act" . flip HashMap.lookup allActs

allActs :: HashMap ActId Act
allActs = HashMap.fromList $ map
  (\a -> (actId $ actAttrs a, a))
  [trapped, theBarrier, whatHaveYouDone]

data Attrs = Attrs
    { actCanComplete :: Bool
    , actId          :: ActId
    , actName        :: Text
    , actSequence    :: Text
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

baseAttrs :: ActId -> Text -> Text -> Attrs
baseAttrs aid name seq' = Attrs { actCanComplete     = False
                               , actId       = aid
                               , actName     = name
                               , actSequence = seq'
                               }

data Act = Trapped TrappedI
    | TheBarrier TheBarrierI
    | WhatHaveYouDone WhatHaveYouDoneI
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

actAttrs :: Act -> Attrs
actAttrs = \case
  Trapped attrs -> coerce attrs
  TheBarrier attrs -> coerce attrs
  WhatHaveYouDone attrs -> coerce attrs

newtype TrappedI = TrappedI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

trapped :: Act
trapped = Trapped . TrappedI $ baseAttrs "01108" "Trapped" "Act 1a"

newtype TheBarrierI = TheBarrierI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

theBarrier :: Act
theBarrier = TheBarrier . TheBarrierI $ baseAttrs "01109" "The Barrier" "Act 2a"

newtype WhatHaveYouDoneI = WhatHaveYouDoneI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

whatHaveYouDone :: Act
whatHaveYouDone = WhatHaveYouDone . WhatHaveYouDoneI $ baseAttrs "01110" "What Have You Done?" "Act 3a"

type ActRunner env = (HasQueue env)

instance (ActRunner env) => RunMessage env Act where
  runMessage msg = \case
    Trapped x -> Trapped <$> runMessage msg x
    TheBarrier x -> TheBarrier <$> runMessage msg x
    WhatHaveYouDone x -> WhatHaveYouDone <$> runMessage msg x

instance (ActRunner env) => RunMessage env TrappedI where
  runMessage msg (TrappedI attrs) = TrappedI <$> runMessage msg attrs

instance (ActRunner env) => RunMessage env TheBarrierI where
  runMessage msg (TheBarrierI attrs) = TheBarrierI <$> runMessage msg attrs

instance (ActRunner env) => RunMessage env WhatHaveYouDoneI where
  runMessage msg (WhatHaveYouDoneI attrs) = WhatHaveYouDoneI <$> runMessage msg attrs

instance (HasQueue env) => RunMessage env Attrs where
  runMessage _msg a@Attrs {..} = pure a
