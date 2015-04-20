module Main (main) where

import Control.Applicative
import Control.Monad
import Data.List

main :: IO ()
main = do
    num <- readLn :: IO Int
    replicateM_ num $ getLine >>= putStrLn . lookAndSay

lookAndSay :: String -> String
lookAndSay = concatMap (liftA2 (++) (show . length) (take 1)) . group
