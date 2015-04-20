module Main (main) where

main :: IO ()
main = do
    stars <- fmap (fst . span (/="0") . lines) getContents
    putStrLn $ if harmonious stars
                  then "Harmonious"
                  else "Cacophonous"

harmonious :: [String] -> Bool
harmonious stars = isPrime (length stars) && all (isPrime . length) stars

-- NOT efficient. Luckily, the inputs are guaranteed to be small
isPrime :: Int -> Bool
isPrime x = null [y | y <- [2 .. floor . sqrt $ fromIntegral x], x `mod` y == 0]
