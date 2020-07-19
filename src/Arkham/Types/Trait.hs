module Arkham.Types.Trait
  ( Trait(..)
  )
where

import           ClassyPrelude
import           Data.Aeson

data Trait = Item
    | Weapon
    | Melee
    | Ally
    | Creature
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON, Hashable)
