module Environment where

import Data.List
import qualified Debug.Trace as Db
import Objects
import Random

data Environment = Environment
  { robots :: [Object],
    kids :: [Object],
    obstacles :: [Object],
    dirt :: [Object],
    playpen :: [Object],
    dimension :: (Int, Int),
    time :: Integer,
    seed :: Int
  }
  deriving (Show)

initEnv =
  Environment
    { robots = [Robot BetaRobot Nothing (1, 1)],
      kids = [Kid (0, 1), Kid (0, 0)],
      obstacles = [Obstacle (1, 0), Obstacle (1, 2), Obstacle (0, 2), Obstacle (0, 3), Obstacle (0, 4)],
      dirt = [Dirt (2, 3)],
      playpen = [Playpen (4, 4)],
      dimension = (5, 5),
      time = 0,
      seed = 5
    }

randomEnv seed childsCount obstaclesCount robotType robotCount m n =
  let p0 = [(i, j) | i <- [0 .. m - 1], j <- [0 .. n - 1]]
      (playpens, p1) = genPlaypen childsCount p0
      s = shuffle seed p1
      (robots, p2) = genRobot robotCount s robotType
      (childs, p3) = genKids childsCount p2
      (obstacle, p4) = genObstacles obstaclesCount p3
      (dirt, _) = genDirt (m * n `div` 2) p4
   in Environment
        { robots = robots,
          kids = childs,
          obstacles = obstacle,
          dirt = dirt,
          playpen = playpens,
          dimension = (m, n),
          time = 0,
          seed = seed
        }
  where
    genRobot count positions robotType = ([Robot robotType Nothing x | x <- take count positions], drop count positions)
    genPlaypen count positions = ([Playpen x | x <- take count positions], drop count positions)
    genDirt count positions = ([Dirt x | x <- take count positions], drop count positions)
    genObstacles count positions = ([Obstacle x | x <- take count positions], drop count positions)
    genKids count positions = ([Kid x | x <- take count positions], drop count positions)

objects :: Environment -> [Object]
objects e = kids e ++ obstacles e ++ dirt e ++ playpen e ++ robots e

objectsAt :: Coord -> Environment -> [Object]
objectsAt c e = filter ((== c) . location) (objects e)

addObject :: Object -> Environment -> Environment
addObject o e = case o of
  Kid coord -> e {kids = Kid coord : kids e}
  Playpen coord -> e {playpen = Playpen coord : playpen e}
  Dirt coord -> e {dirt = Dirt coord : dirt e}
  Obstacle coord -> e {obstacles = Obstacle coord : obstacles e}
  Robot a b coord -> e {robots = Robot a b coord : robots e}

removeObject :: Object -> Environment -> Environment
removeObject o e = case o of
  Kid coord -> e {kids = delete (Kid coord) (kids e)}
  Playpen coord -> e {playpen = delete (Playpen coord) (playpen e)}
  Dirt coord -> e {dirt = delete (Dirt coord) (dirt e)}
  Obstacle coord -> e {obstacles = delete (Obstacle coord) (obstacles e)}
  Robot a b coord -> e {robots = delete (Robot a b coord) (robots e)}

moveObject :: Object -> Object -> Environment -> Environment
moveObject b a e = addObject a $ removeObject b e

moveObjectToCoord (Robot t o c) nc = moveObject (Robot t o c) (Robot t o nc)
moveObjectToCoord (Kid c) nc = moveObject (Kid c) (Kid nc)
moveObjectToCoord (Obstacle c) nc = moveObject (Obstacle c) (Obstacle nc)
moveObjectToCoord (Playpen c) nc = moveObject (Playpen c) (Playpen nc)
moveObjectToCoord (Dirt c) nc = moveObject (Dirt c) (Dirt nc)

validPos :: Coord -> Environment -> Bool
validPos (x, y) e = let (n, m) = dimension e in y >= 0 && y < m && x < n && x >= 0

adjacentObjects :: Object -> Environment -> [Object]
adjacentObjects o e =
  concat
    [ objectsAt x e | x <- adjacentCoords (location o), validPos x e
    ]

adjacentObjectsCoords :: Coord -> Environment -> [Object]
adjacentObjectsCoords c e =
  concat
    [ objectsAt x e | x <- adjacentCoords c, validPos x e
    ]

adjacentEmpty :: Object -> Environment -> [Coord]
adjacentEmpty o e = [x | x <- adjacentCoords (location o), validPos x e, empty x]
  where
    empty c = null $ objectsAt c e

adjacentEmptyCoords :: Coord -> Environment -> [Coord]
adjacentEmptyCoords c e = [x | x <- adjacentCoords c, validPos x e, empty x]
  where
    empty c = null $ objectsAt c e

adjacentAccesibleCoords :: Coord -> Environment -> [Coord]
adjacentAccesibleCoords c e = [x | x <- adjacentCoords c, validPos x e, accesible x]
  where
    accesible x =
      let objs = objectsAt x e
       in null objs || (length objs == 1 && (isDirt (head objs) || isPlaypen (head objs)))

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
           in if decisions > 0
                then
                  if decision < length empty
                    then generateDirt (location kid) $ moveObject kid (Kid (empty !! decision)) env {seed = s1}
                    else
                      let obst = obstacles !! (decision - length empty)
                          dir = direction (location kid) (location obst)
                          (succeed, newEnv) = moveObstacle dir obst env {seed = s1}
                       in if succeed
                            then generateDirt (location kid) $ moveObject kid (Kid (location obst)) newEnv
                            else env {seed = s1}
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
          genDirt = even s
          emptyPositions = adjacentEmpty (Kid center) env
       in if count > 0 && genDirt && not (null emptyPositions)
            then
              let s1 = runRandom rand s
                  position = emptyPositions !! mod s1 (length emptyPositions)
                  newEnv = addObject (Dirt position) env {seed = s1}
               in placeDirt (count - 1) newEnv
            else env {seed = s}
