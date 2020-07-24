module Arkham.Types.Investigator.Runner where

import Arkham.Types.ActId
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Query

type InvestigatorRunner env
  = ( HasCount ClueCount LocationId env
    , HasSet DamageableAssetId InvestigatorId env
    , HasQueue env
    , HasSet AdvanceableActId () env
    , HasSet ConnectedLocationId LocationId env
    , HasSet BlockedLocationId () env
    )
