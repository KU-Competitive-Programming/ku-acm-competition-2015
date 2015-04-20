module Main (main) where

import Control.Monad
import Data.Char

main :: IO ()
main = do
  n <- readLn :: IO Int
  replicateM_ n $ do
    line <- getLine
    putStrLn $ rot13 line

shift :: Char -> Char
shift c | isAsciiLower c = shifted 'a'
        | isAsciiUpper c = shifted 'A'
        | otherwise = c
  where
    shifted a = chr $ ((ord c - ord a + delta) `mod` range) + ord a
    range = ord 'z' - ord 'a' + 1
    delta = 13

rot13 :: String -> String
rot13 = map shift
