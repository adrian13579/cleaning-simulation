module Environment where

import Data.List
import Objects

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
    { robots = [],
      kids = [Kid (0, 0)],
      obstacles = [Obstacle (1, 0), Obstacle (0, 1), Obstacle (1, 2), Obstacle (0, 2)],
      -- obstacles = [],
      dirt = [],
      playpen = [],
      dimension = (3, 5),
      time = 0,
      seed = 1
      -- seed = 5
    }

randomEnv :: Int -> Environment
randomEnv seed = initEnv

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

