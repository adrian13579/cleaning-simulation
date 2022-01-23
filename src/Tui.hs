module Tui where

import Environment
import Objects

getRow r e = [charObject (r, c) | c <- [1 .. 3]]
  where
    charObject c =
      let o = objectsAt c e
       in case o of
            [Kid a] -> 'K'
