module Tui where

import Environment
import Objects

printPrettyEnvironment env =
  let envMap = concat [getRow r | r <- [0 .. fst (dimension env) - 1]]
      envTime = "time: " ++ show (time env) ++ "\n"
      envSeed = "seed: " ++ show (seed env) ++ "\n"
   in envMap ++ envTime ++ envSeed
  where
    getRow r = concat [charObject (r, c) | c <- [0 .. snd (dimension env) - 1]] ++ "\n"
    charObject coord =
      let charObjects = concat [f x | x <- objectsAt coord env]
       in case length charObjects of
            0 -> "    " ++ show coord ++ " "
            1 -> " " ++ charObjects ++ "  " ++ show coord ++ " "
            2 -> " " ++ charObjects ++ " " ++ show coord ++ " "
            _ -> " " ++ charObjects ++ show coord ++ " "
    f o = case o of
      Kid _ -> "K"
      Obstacle _ -> "O"
      Dirt _ -> "D"
      Robot AlphaRobot ( Just (Kid _) ) _-> "Ak"
      Robot AlphaRobot Nothing _ -> "A"
      Robot BetaRobot ( Just (Kid _) ) _-> "Bk"
      Robot BetaRobot Nothing _ -> "B"
      Playpen _ -> "P"
