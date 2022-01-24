module Simulation where

import Environment
import Objects
import Random

kidAction kid e =
  let s = runRandom rand (seed e)
   in if even s || any isPlaypen (objectsAt (location kid) e)
        then e {seed = s}
        else
          let obstacles = filter isObstacle (adjacentObjects kid e)
              empty = adjacentEmpty kid e
              decisions = length empty + length obstacles
              newSeed = runRandom rand (seed e)
              decision = mod newSeed decisions
           in if decision < length empty
                then moveObject kid (Kid (empty !! decision)) e {seed = newSeed}
                else
                  let decision = decision - length empty - 1
                      o = obstacles !! decision
                      d = direction (location kid) (location o)
                      (succeed, newEnv) = moveObstacle d o e
                   in if succeed
                        then moveObject kid (Kid (location o)) newEnv
                        else e

moveObstacle d o e =
  let position = adjacentCoords (location o) !! d
   in if not (validPos position e)
        then (False, e)
        else
          if position `elem` adjacentEmpty o e
            then (True, moveObject o (Obstacle position) e)
            else
              let adjObstacles = filter isObstacle (objectsAt position e)
               in if null adjObstacles
                    then (False, e)
                    else
                      let adjObstacle = head adjObstacles
                          (succeed, newEnv) = moveObstacle d adjObstacle e
                       in if succeed
                            then (True, moveObject o (Obstacle position) newEnv)
                            else (False, e)

-- placeDirt c p e =
