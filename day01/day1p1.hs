#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
main :: IO ()
main = interact $ show . sum . map (\(a,b) -> if a==b then (read :: String -> Int) [a] else 0) . (\ss -> zip ss (tail ss)) . (\ss -> ss ++ [head ss]) . head . lines