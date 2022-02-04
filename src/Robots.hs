module Robots where

import Data.List ()
import Data.Matrix
import Environment
import Objects
import Random ()
import Utils (mkUniq)

robotAction :: Object -> Environment -> Environment
robotAction robot env = case robot of
  Robot AlphaRobot _ _ -> alphaRobotAction robot env
  Robot BetaRobot _ _ -> betaRobotAction robot env

betaRobotAction robot env =
  let (dist, objects) = scanEnv (location robot) env
   in case robot of
        Robot BetaRobot (Just (Kid _)) coord ->
          let playpen = [x | x <- objects, freePlaypen x robot env]
              dirt = [x | x <- objects, isDirtOnly x robot env]
              targetPlaypen = head playpen
              targetDirt = head dirt
           in if null playpen
                then
                  if null dirt
                    then env
                    else
                      let path = findPath coord (location targetDirt) dist env
                       in case length path of
                            0 -> cleanDirt targetDirt env
                            1 -> moveObjectToCoord robot (head path) env
                            _ -> moveObjectToCoord robot (path !! 1) env
                else
                  let path = findPath coord (location targetPlaypen) dist env
                   in case length path of
                        0 -> dropKid robot env
                        1 -> moveObjectToCoord robot (head path) env
                        _ -> moveObjectToCoord robot (path !! 1) env
        Robot BetaRobot Nothing coord ->
          let kids = [x | x <- filter (/= robot) objects, freeKid x env]
              dirt = [x | x <- filter (/= robot) objects, isDirtOnly x robot env]
           in if null kids
                then
                  if null dirt
                    then env
                    else
                      let first = head dirt
                          path = findPath coord (location first) dist env
                       in case length path of
                            0 -> cleanDirt first env
                            _ -> moveObjectToCoord robot (head path) env
                else
                  let first = head kids
                      path = findPath coord (location first) dist env
                   in case length path of
                        0 -> case first of
                          Kid _ -> snd $ carryKid robot first env
                          _ -> env
                        1 -> case first of
                          Kid _ ->
                            let (robotWithKid, env1) = carryKid robot first env
                             in moveObjectToCoord robotWithKid (head path) env1
                          _ -> env
                        _ -> moveObjectToCoord robot (head path) env

alphaRobotAction robot env =
  let (dist, objects) = scanEnv (location robot) env
   in case robot of
        Robot AlphaRobot (Just (Kid _)) coord ->
          let playpen = [x | x <- objects, freePlaypen x robot env]
              dirt = [x | x <- objects, isDirtOnly x robot env]
              targetPlaypen = head playpen
              targetDirt = head dirt
           in if null playpen
                then
                  if null dirt
                    then env
                    else
                      let path = findPath coord (location targetDirt) dist env
                       in case length path of
                            0 -> cleanDirt targetDirt env
                            1 -> moveObjectToCoord robot (head path) env
                            _ -> moveObjectToCoord robot (path !! 1) env
                else
                  let path = findPath coord (location targetPlaypen) dist env
                   in case length path of
                        0 -> dropKid robot env
                        1 -> moveObjectToCoord robot (head path) env
                        _ -> moveObjectToCoord robot (path !! 1) env
        Robot AlphaRobot Nothing coord ->
          let decisions = [x | x <- filter (/= robot) objects, freeKid x env || isDirtOnly x robot env]
           in if null decisions
                then env
                else
                  let first = head decisions
                      path = findPath coord (location first) dist env
                   in case length path of
                        0 -> case first of
                          Kid _ -> snd $ carryKid robot first env
                          Dirt _ -> cleanDirt first env
                          _ -> env
                        1 -> case first of
                          Kid _ ->
                            let (robotWithKid, env1) = carryKid robot first env
                             in moveObjectToCoord robotWithKid (location first) env1
                          Dirt _ -> moveObjectToCoord robot (location first) env
                          _ -> env
                        _ -> moveObjectToCoord robot (head path) env

freeKid (Kid coord) env =
  not $ any isPlaypen (objectsAt coord env)
freeKid _ _ = False

isDirtOnly (Dirt coord) robot env =
  null [x | x <- objectsAt coord env, x /= robot, x /= Dirt coord]
isDirtOnly _ _ _ = False

freePlaypen (Playpen coord) robot env =
  null [x | x <- objectsAt coord env, x /= robot, x /= Playpen coord]
freePlaypen _ _ _ = False

carryKid (Robot t o c) kid env =
  let env1 = removeObject kid env
      env2 = removeObject (Robot t o c) env1
      env3 = addObject (Robot t (Just kid) c) env2
   in (Robot t (Just kid) c, env3)

dropKid (Robot t o c) env =
  let env1 = addObject (Kid c) env
      env2 = removeObject (Robot t o c) env1
      env3 = addObject (Robot t Nothing c) env2
   in env3

cleanDirt (Dirt coord) = removeObject (Dirt coord)

scanEnv :: Coord -> Environment -> (Matrix Int, [Object])
scanEnv position env =
  let (n, m) = dimension env
      v = matrix n m (\_ -> m * n)
   in nearestObjects 1 [position] v (objectsAt position env)
  where
    nearestObjects distance queue visited objects =
      let adjacents = mkUniq $ nonVisitedAdjCoord queue visited
          newVisited = setDistance distance adjacents visited
          newObjects = visitedObjects (queue ++ adjacents) objects
       in if null adjacents
            then (visited, newObjects)
            else nearestObjects (distance + 1) adjacents newVisited newObjects

    nonVisitedAdjCoord [] _ = []
    nonVisitedAdjCoord (x : xs) visited =
      [ (i, j) | (i, j) <- adjacentAccesibleCoords x env, visited ! (i + 1, j + 1) == uncurry (*) (dimension env)
      ]
        ++ nonVisitedAdjCoord xs visited

    visitedObjects [] obj = obj
    visitedObjects (x : xs) obj =
      let newObj = [o | o <- adjacentObjectsCoords x env, o `notElem` obj]
       in visitedObjects xs (obj ++ newObj)

    setDistance _ [] m = m
    setDistance v ((i, j) : xs) m = setDistance v xs (setElem v (i + 1, j + 1) m)

findPath source dest distances env =
  if source == dest
    then []
    else reverse $ findPathReverse dest source distances env [dest]
  where
    findPathReverse source dest distances env path =
      let adjacents = [x | x <- adjacentCoords source, validPos x env]
          (_, next) = minimum ([(distances ! (i + 1, j + 1), (i, j)) | (i, j) <- adjacents])
       in if dest `elem` adjacents
            then path
            else findPathReverse next dest distances env (path ++ [next])
