module Simulation where

import qualified Debug.Trace as Db
import Environment
import Robots
import Tui

runSimulation env limitTime =
  if time env == limitTime -- TODO: or 60% clean
    then env
    else
      let env1 = Db.trace ("Agents:\n" ++ printPrettyEnvironment (nextStateAgent env)) nextStateAgent env
          env2 = Db.trace ("Environment:\n" ++ printPrettyEnvironment (nextStateEnv env1)) nextStateEnv env1
       in Db.trace
            (" Environment State:\n" ++ printPrettyEnvironment env2 {time = time env + 1})
            runSimulation
            env2 {time = time env + 1}
            limitTime

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
      if Db.trace ("hola" ++ show ( index == length ( robots env ) )) index == length (robots env)
        then env
        else nextStateAgentAux (robotAction (robots env !! index) env) (index + 1)
