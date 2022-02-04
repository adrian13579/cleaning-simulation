module Tui where

import Environment
import Objects
import Utils

printPrettyEnvironment env =
  let envMap = concat [getRow r | r <- [0 .. fst (dimension env) - 1]]
      envTime = "time: " ++ show (time env) ++ "\n"
      clean = "clean: " ++ show (round (100 - percent part total)) ++ "%"
   in envMap ++ envTime ++ clean
  where
    total = let (m, n) = dimension env in fromIntegral (m * n)
    part = fromIntegral $ length (dirt env)
    getRow r = concat [charObject (r, c) | c <- [0 .. snd (dimension env) - 1]] ++ "\n"
    charObject coord =
      let charObjects = concat [f x | x <- objectsAt coord env]
       in case length charObjects of
            0 -> "    " ++ "|"
            1 -> " " ++ charObjects ++ "  " ++ "|"
            2 -> " " ++ charObjects ++ " " ++ "|"
            _ -> " " ++ charObjects ++ "|"
    f o = case o of
      Kid _ -> "K"
      Obstacle _ -> "Ob"
      Dirt _ -> "D"
      Robot AlphaRobot (Just (Kid _)) _ -> "Ak"
      Robot AlphaRobot Nothing _ -> "A"
      Robot BetaRobot (Just (Kid _)) _ -> "Bk"
      Robot BetaRobot Nothing _ -> "B"
      Playpen _ -> "P"
