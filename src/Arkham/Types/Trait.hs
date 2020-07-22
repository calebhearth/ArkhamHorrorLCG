module Arkham.Types.Trait
  ( Trait(..)
  )
where

import ClassyPrelude
import Data.Aeson

data Trait
  = Agency
  | Ally
  | Armor
  | Artist
  | Assistant
  | Augury
  | Avatar
  | Believer
  | Blessed
  | Blunder
  | Bold
  | Boon
  | Charm
  | Chosen
  | Civic
  | Clairvoyant
  | Clothing
  | Composure
  | Condition
  | Connection
  | Conspirator
  | Creature
  | Criminal
  | Cultist
  | Curse
  | Cursed
  | DarkYoung
  | DeepOne
  | Desperate
  | Detective
  | Developed
  | Dreamer
  | Dreamlands
  | Drifter
  | Elite
  | Endtimes
  | Evidence
  | Expert
  | Extradimensional
  | Fated
  | Favor
  | Firearm
  | Flaw
  | Footwear
  | Fortune
  | Gambit
  | Geist
  | Ghoul
  | Grant
  | Humanoid
  | Hunter
  | Illicit
  | Improvised
  | Injury
  | Innate
  | Insight
  | Instrument
  | Item
  | Job
  | Lunatic
  | Madness
  | Medic
  | Melee
  | Miskatonic
  | Monster
  | Mystery
  | Occult
  | Omen
  | Pact
  | Paradox
  | Patron
  | Performer
  | Police
  | Practiced
  | Ranged
  | Relic
  | Reporter
  | Research
  | Ritual
  | Scholar
  | Science
  | Serpent
  | Service
  | SilverTwilight
  | Socialite
  | Song
  | Sorcerer
  | Spell
  | Spirit
  | Summon
  | Supply
  | Syndicate
  | Tactic
  | Talent
  | Tarot
  | Task
  | Tindalos
  | Tome
  | Tool
  | Trap
  | Trick
  | Upgrade
  | Veteran
  | Warden
  | Wayfarer
  | Weapon
  | Witch
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
