module Main (main) where

import Data.Char (digitToInt)
import Data.Foldable (for_)

main :: IO ()
main = do
    n <- readLn :: IO Int
    for_ [1..n] . const $ getLine >>= stuff

stuff :: String -> IO ()
stuff input =
    let [ln, fn, visa, pob] = words input
        visaSum = sum $ map digitToInt visa
     in putStrLn $ if visaSum == 25 || pob `elem` ["BYTELANDIA", "FLATLAND", "LEROY", "JENKINS"]
                      then "ENTRY DENIED"
                      else "CAUSE NO TROUBLE " ++ fn ++ " " ++ ln
