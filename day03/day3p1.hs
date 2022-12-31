#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
main :: IO ()
main = interact $ show . solve . read

solve :: Int -> Int
solve pos = let sq = floor $ sqrt (fromIntegral pos)
                circum = ((sq+1)*(sq+1) - sq*sq) `div` 4
            in (sq-1) + abs (pos `mod` circum - (circum `div` 2stast))