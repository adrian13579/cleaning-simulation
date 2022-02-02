module Simulation where

import Data.Matrix
import qualified Debug.Trace as Db
import Environment
import Objects
import Random
import Utils (mkUniq)

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
           in if decision < Db.trace (show $ length empty) length empty
                then generateDirt (location kid) $ moveObject kid (Kid (empty !! decision)) env {seed = s1}
                else
                  let obst = obstacles !! (decision - length empty)
                      dir = direction (location kid) (location obst)
                      (succeed, newEnv) = moveObstacle dir obst env {seed = s1}
                   in if succeed
                        then generateDirt (location kid) $ moveObject kid (Kid (location obst)) newEnv
                        else env {seed = s1}

moveObstacle :: Int -> Object -> Environment -> (Bool, Environment)
moveObstacle dir obst env =
  let position = Db.trace "Moving obstacles ..." adjacentCoords (location obst) !! dir
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

generateDirt :: Coord -> Environment -> Environment
generateDirt center env =
  let kidsCount = length $ filter isKid (adjacentObjects (Kid center) env)
   in case kidsCount of
        1 -> placeDirt 1 env
        2 -> placeDirt 3 env
        _ -> placeDirt 6 env
  where
    placeDirt count env =
      let s = runRandom rand (seed env)
          genDirt = Db.trace "Generating dirt..." even s
          emptyPositions = adjacentEmpty (Kid center) env
       in if count > 0 && genDirt && not (null emptyPositions)
            then
              let s1 = runRandom rand s
                  position = emptyPositions !! mod s1 (length emptyPositions)
                  newEnv = addObject (Dirt position) env {seed = s1}
               in placeDirt (count - 1) newEnv
            else env {seed = s}

scanEnv :: Coord -> Environment -> Matrix Int
scanEnv position env =
  let (n, m) = dimension env
      v = matrix n m (\_ -> m * n)
   in nearestObjects 1 [position] v
  where
    nearestObjects distance queue visited =
      let adjacents = mkUniq $ nonVisitedAdjCoord queue visited
          newVisited = setDistance distance adjacents visited
       in if null adjacents
            then visited
            else nearestObjects (distance + 1) adjacents newVisited
    nonVisitedAdjCoord [] _ = []
    nonVisitedAdjCoord (x : xs) visited =
      [ (i, j) | (i, j) <- adjacentEmptyCoords x env, visited ! (i + 1, j + 1) == uncurry (*) (dimension env)
      ]
        ++ nonVisitedAdjCoord xs visited
    setDistance _ [] m = m
    setDistance v ((i, j) : xs) m = setDistance v xs (setElem v (i + 1, j + 1) m)

findPath :: Coord -> Coord -> Matrix Int -> Environment -> [Coord] -> [Coord]
findPath source dest distances env path =
  let adjacents = [x | x <- adjacentCoords source, validPos x env]
      (_, next) = minimum ([(distances ! (i + 1, j + 1), (i, j)) | (i, j) <- adjacents])
   in if dest `elem` adjacents
        then path
        else findPath next dest distances env (next : path)
