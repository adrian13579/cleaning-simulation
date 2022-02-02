module Robots where

import Data.Matrix
import qualified Debug.Trace as Db
import Environment
import Objects
import Random
import Utils (mkUniq)

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
