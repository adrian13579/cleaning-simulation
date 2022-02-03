module Main where

import Environment
import Simulation

main :: IO ()
main = print $ runSimulation initEnv 500
