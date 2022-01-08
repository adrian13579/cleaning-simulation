module Main where
import Test ( test )


main :: IO ()
main = print ( until (>100) (*7) 1)  



