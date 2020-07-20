module Arkham.Types.SkillType where

import           ClassyPrelude
import           Data.Aeson

data SkillType = SkillWillpower
    | SkillIntellect
    | SkilLCombat
    | SkillAgility
    | SkillWild
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
