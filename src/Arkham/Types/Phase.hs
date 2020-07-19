module Arkham.Types.Phase
  ( Phase(..)
  )
where

import           ClassyPrelude
import           Data.Aeson

data Phase = Mythos
    | Investigation
    | Enemy
    | Upkeep
    | Resolution
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
