module Simulation where

import Environment
import Objects
import Random

kidAction :: Object -> Environment -> Environment
kidAction kid env =
  let s = runRandom rand (seed env)
   in if even s || any isPlaypen (objectsAt (location kid) env)
        then env {seed = s}
        else
          let obstacles = filter isObstacle (adjacentObjects kid env)
              empty = adjacentEmpty kid env
              decisions = length empty + length obstacles
              s1 = runRandom rand (seed env)
              decision = mod s1 decisions
           in if decision < length empty
                then moveObject kid (Kid (empty !! decision)) env {seed = s1}
                else
                  let decision = decision - length empty - 1
                      obst = obstacles !! decision
                      dir = direction (location kid) (location obst)
                      (succeed, newEnv) = moveObstacle dir obst env
                   in if succeed
                        then moveObject kid (Kid (location obst)) newEnv
                        else env

moveObstacle :: Int -> Object -> Environment -> (Bool, Environment)
moveObstacle dir obst env =
  let position = adjacentCoords (location obst) !! dir
   in if not (validPos position env)
        then (False, env)
        else
          if position `elem` adjacentEmpty obst env
            then (True, moveObject obst (Obstacle position) env)
            else
              let adjObstacles = filter isObstacle (objectsAt position env)
               in if null adjObstacles
                    then (False, env)
                    else
                      let adjObstacle = head adjObstacles
                          (succeed, newEnv) = moveObstacle dir adjObstacle env
                       in if succeed
                            then (True, moveObject obst (Obstacle position) newEnv)
                            else (False, env)

generateDirt :: Coord ->  Environment -> Environment
generateDirt center  env = 
  let kidsCount = length $ filter isKid (adjacentObjects  ( Kid center ) env)
    in case kidsCount of
         1 -> placeDirt 1 env
         2 -> placeDirt 3 env
         _ -> placeDirt 6 env
  where
    placeDirt count env =
      let s = runRandom rand (seed env)
          genDirt = even s
          emptyPositions = adjacentEmpty (Kid center) env
       in if count>0 && genDirt && not (null emptyPositions)
            then
              let s1 = runRandom rand s
                  position = emptyPositions !! mod s1 (length emptyPositions)
                  newEnv = addObject ( Dirt position ) env
               in placeDirt ( count - 1 ) newEnv
            else env
