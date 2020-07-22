module Main where

import Arkham.Types.Game
import Arkham.Types.Investigator
import ClassyPrelude
import Text.Pretty.Simple

main :: IO ()
main = do
  ge <- runGame =<< newGame "01104" investigators
  pPrint ge
  where investigators = [lookupInvestigator "01001"]
