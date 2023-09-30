#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
main :: IO ()
main = interact $ show . sum . map (\(a,b) -> a * b) . filter scannerOverlap . map parse . lines

parse :: String -> (Int, Int)
parse s = let a = read . takeWhile (/= ':') $ s
              b = read . tail . tail . dropWhile (/= ':') $ s
          in (a, b)

scannerOverlap :: (Int, Int) -> Bool
scannerOverlap (a,b) = (a `mod` (2 * (b - 1))) == 0
