#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
main :: IO ()
main = interact $ show . findPerfectDelay . map parse . lines

parse :: String -> (Int, Int)
parse s = let a = read . takeWhile (/= ':') $ s
              b = read . tail . tail . dropWhile (/= ':') $ s
          in (a, b)

findPerfectDelay :: [(Int, Int)] -> Int
findPerfectDelay ps = testWithDelay 0
    where
        testWithDelay i = if null . filter (scannerOverlap i) $ ps
                          then i
                          else testWithDelay (i+1)

scannerOverlap :: Int -> (Int, Int) -> Bool
scannerOverlap d (a,b) = ((a + d) `mod` (2 * (b - 1))) == 0
