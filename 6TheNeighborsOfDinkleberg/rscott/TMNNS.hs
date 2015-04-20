module Main (main) where

import Data.Foldable as F
import Data.Map (Map, (!), fromList, fromListWith, union)

main :: IO ()
main = do
    n <- readLn :: IO Int
    for_ [1..n] . const $ do
        g <- fmap parseGraph getLine
        for_ g $ putStr . (++ " ") . show . F.sum . map (metroNum g)
        putStrLn ""

type Graph = Map Int [Int]

metroNum :: Graph -> Int -> Int
metroNum g i = length $ g ! i

-- tmnns :: Graph -> Int -> Int
-- tmnns g i = sum . map (metroNum g) $ g ! i

parseGraph :: String -> Graph
parseGraph s =
  let ws = map read $ words s
      c  = ws !! 0
      -- h  = read $ ws !! 1
      es = pairify $ drop 2 ws
      t1 = fromListWith (++) es
      t2 = fromList . zip [1..c] $ repeat []
   in union t1 t2

pairify :: [a] -> [(a, [a])]
pairify (a:b:cs) = (a,[b]) : (b, [a]) : pairify cs
pairify []       = []
pairify _        = error "odd number of elements"
