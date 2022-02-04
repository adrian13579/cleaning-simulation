module Simulation where

import qualified Debug.Trace as Db
import Environment
import Robots
import Tui
import Utils

runSimulation env limitTime =
  if time env == limitTime || percent part total <= 40
    then Db.trace "Final State:\n" env
    else
      let env1 = Db.trace ("Agents' Turn:\n" ++ printPrettyEnvironment (nextStateAgent env)) nextStateAgent env
          env2 = Db.trace ("Environment's Turn:\n" ++ printPrettyEnvironment (nextStateEnv env1)) nextStateEnv env1
       in runSimulation env2 {time = time env + 1} limitTime
  where
    total = let (m, n) = dimension env in fromIntegral (m * n)
    part = fromIntegral $ length (dirt env)

nextStateEnv env =
  nextStateEnvAux env 0
  where
    nextStateEnvAux env index =
      if index == length (kids env)
        then env
        else nextStateEnvAux (kidAction (kids env !! index) env) (index + 1)

nextStateAgent env =
  nextStateAgentAux env 0
  where
    nextStateAgentAux env index =
      if  index == length (robots env)
        then env
        else nextStateAgentAux (robotAction (robots env !! index) env) (index + 1)
