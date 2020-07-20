module Arkham.Types.Event
  ( allEvents
  )
where

import           Arkham.Types.Card
import           Arkham.Types.InvestigatorId
import           Arkham.Types.Message
import           ClassyPrelude
import qualified Data.HashMap.Strict         as HashMap

allEvents :: HashMap CardCode (InvestigatorId -> [Message])
allEvents = HashMap.fromList [("01088", \iid -> [TakeResources iid 3])]
