module Arkham.Types.Helpers where

import           ClassyPrelude

without :: Int -> [a] -> [a]
without n as = [ a | (i, a) <- zip [0 ..] as, i /= n ]

infix 9 !!?
(!!?) :: [a] -> Int -> Maybe a
(!!?) xs i | i < 0     = Nothing
           | otherwise = go i xs
 where
  go :: Int -> [a] -> Maybe a
  go 0 (x : _ ) = Just x
  go j (_ : ys) = go (j - 1) ys
  go _ []       = Nothing
{-# INLINE (!!?) #-}
