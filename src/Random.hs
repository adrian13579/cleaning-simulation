module Random where

import State
import System.Random

type R a = State StdGen a

runRandom :: R Int -> Int -> Int
runRandom action seed = evalState action $ mkStdGen seed

rand :: R Int
rand = do
  gen <- get
  let (r, gen') = random gen
  put gen'
  return r

randTest :: Int -> Int
randTest i = fst (next (mkStdGen i))

shuffle seed = shuffleAux (mkStdGen seed)

shuffleAux _ [] = []
shuffleAux gen list = randomElem : shuffleAux newGen newList
  where
    randomTuple = randomR (0, length list - 1) gen
    randomIndex = fst randomTuple
    newGen = snd randomTuple
    randomElem = list !! randomIndex
    newList = take randomIndex list ++ drop (randomIndex + 1) list
