module Main where

import Environment
import Simulation
import Tui
import Objects

main :: IO ()
main =
  putStr $
    printPrettyEnvironment
      (runSimulation (randomEnv 5 20 5 BetaRobot 3 20 20) 5000 )
