module Objects where

type Coord = (Int, Int)

adjacentCoords :: Coord -> [Coord]
adjacentCoords (x, y) =
  [ (x, y + 1),
    (x + 1, y + 1),
    (x + 1, y),
    (x + 1, y - 1),
    (x, y - 1),
    (x - 1, y - 1),
    (x - 1, y)
  ]

data Object
  = Kid Coord
  | Obstacle Coord
  | Dirt Coord
  | Playpen Coord
  | Robot RobotType (Maybe Object) Coord
  deriving (Show, Eq)

data RobotType = AlphaRobot | BetaRobot
  deriving (Show, Eq)

samePos :: Object -> Object -> Bool
samePos a b = location a == location b

location :: Object -> Coord
location a = case a of
  Robot _ _ b -> b
  Kid b -> b
  Obstacle b -> b
  Dirt b -> b
  Playpen b -> b
