module Main (main) where

import Data.Bits
import Data.Char
import Data.Foldable
import Data.Maybe
import Numeric

main :: IO ()
main = do
    n <- readLn :: IO Int
    for_ [1..n] . const $ do
        putStrLn "wat"

toBit :: Char -> Int
toBit 'A' = 0
toBit 'B' = 1
toBit _   = error "wat"

readBin :: String -> Int
readBin = fromJust . fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt

adjacent :: Int -> Int -> Int -> Bool
adjacent w1 w2 bits =
    let diff = w1 `xor` w2
     in if diff /= 0
             && ((diff .&. (-diff)) `xor` diff) == 0
           then if w1 .&. (diff - 1) == diff `shiftR` 1
                     || w1 .&. w2 == 0
                     && w1 .|. w2 == 1 `shiftR` bits
                   then True
                   else False
           else False
