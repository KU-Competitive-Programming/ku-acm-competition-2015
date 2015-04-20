module Main (main) where

import Data.Foldable as F
import Data.List

main :: IO ()
main = do
    n <- readLn :: IO Int
    for_ [1..n] . const $ do
        w <- getLine
        putStrLn . show $ lexiPos w

factorial :: Int -> Integer
factorial n = F.product [1 .. toInteger n]

multiperms :: String -> Integer
multiperms xs = factorial (F.sum ys) `div` F.product (map factorial ys) where
  ys = map length . group . sort $ xs

lexiPos :: String -> Integer
lexiPos [_] = 1
lexiPos xs'@(x:xs) = F.sum (map multiperms ys) + lexiPos xs where
  ys = nub . map (sort . tail) . filter (\(z:_) -> z < x) $ zipWith (\i t -> head t : i ++ tail t ) (init . tail $ inits xs') (tail . init $ tails xs')
lexiPos _ = error "wat"
