module Utils where

import Data.Set 

mkUniq :: Ord a => [a] -> [a]
mkUniq = toList . fromList



