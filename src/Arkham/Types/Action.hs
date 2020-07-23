module Arkham.Types.Action where

import ClassyPrelude
import Data.Aeson

data Action
  = Move
  | Investigate
  | Fight
  | Evade
  | Engage
  | Draw
  | Resource
  | Play
  | Ability
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
