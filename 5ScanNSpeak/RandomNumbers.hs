module Main (main) where

import System.Random

main :: IO ()
main = printRandomNums $ (2 :: Int)^(16 :: Int)

printRandomNums :: Int -> IO ()
printRandomNums n = do
    if n <= 0
       then return ()
       else do randInt <- randomIO :: IO Int
               if (randInt > 0)
                  then print randInt >> printRandomNums (n-1)
                  else printRandomNums n
