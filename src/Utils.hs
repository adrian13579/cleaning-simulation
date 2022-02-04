module Utils where

import Data.Set

mkUniq :: Ord a => [a] -> [a]
mkUniq = toList . fromList

percent part total = 100 * part / total
