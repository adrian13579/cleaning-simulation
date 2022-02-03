module Objects where

import Data.List
import Data.Maybe

type Coord = (Int, Int)

adjacentCoords :: Coord -> [Coord]
adjacentCoords (x, y) =
  [ (x, y + 1),
    (x + 1, y + 1),
    (x + 1, y),
    (x + 1, y - 1),
    (x - 1, y + 1),
    (x, y - 1),
    (x - 1, y - 1),
    (x - 1, y)
  ]

direction a b = fromMaybe 10 $ elemIndex b (adjacentCoords a)

data Object
  = Kid Coord
  | Obstacle Coord
  | Dirt Coord
  | Playpen Coord
  | Robot RobotType (Maybe Object) Coord
  deriving (Show, Eq, Ord)

data RobotType = AlphaRobot | BetaRobot
  deriving (Show, Eq, Ord)

getKid ( Robot _ k _ ) = k

samePos :: Object -> Object -> Bool
samePos a b = location a == location b

location :: Object -> Coord
location a = case a of
  Robot _ _ b -> b
  Kid b -> b
  Obstacle b -> b
  Dirt b -> b
  Playpen b -> b

isObstacle (Obstacle _) = True
isObstacle _ = False

isPlaypen (Playpen _) = True
isPlaypen _ = False

isKid ( Kid _ ) = True
isKid _ = False

isDirt ( Dirt _ ) = True
isDirt _ = False
