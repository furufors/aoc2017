#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
main :: IO ()
main = interact $ show . countGarbage

countGarbage :: String -> Int
countGarbage = countGarbage' False
    where
        countGarbage' b [] = 0
        countGarbage' False ('<':ss) = countGarbage' True ss
        countGarbage' True ('!':ss) = countGarbage' True (tail ss)
        countGarbage' True ('>':ss) = countGarbage' False ss
        countGarbage' True (_:ss) = 1 + countGarbage' True ss
        countGarbage' False (s:ss) = countGarbage' False ss
