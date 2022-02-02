module Main where

import Debug.Trace
import Environment
import Objects
import Simulation
import Tui
import Data.Matrix

{- main :: IO ()
main = putStr $ printPrettyEnvironment (kidAction (Kid (0, 0)) initEnv) -}

{- main :: IO ()
main = print $ scanEnv (0, 0) initEnv   -}

{- main :: IO ()
main = print $ adjacentCoords (2, 2)-- initEnv  -}

main :: IO ()
main = print $ findPath (2,4) (0,0) ( scanEnv (0, 0) initEnv ) initEnv []  

-- main = print $ adjacentEmptyCoords (2,4) initEnv


