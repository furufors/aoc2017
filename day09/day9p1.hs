#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
main :: IO ()
main = interact $ show . scoreBrackets . removeGarbage

scoreBrackets :: String -> Int
scoreBrackets = scoreBrackets' 0
    where
        scoreBrackets' n [] = 0
        scoreBrackets' n ('{':ss) = (n+1) + scoreBrackets' (n+1) ss
        scoreBrackets' n ('}':ss) = scoreBrackets' (n-1) ss
        scoreBrackets' n (_:ss) = scoreBrackets' n ss

removeGarbage :: String -> String
removeGarbage = removeGarbage' False
    where
        removeGarbage' b [] = []
        removeGarbage' False ('<':ss) = removeGarbage' True ss
        removeGarbage' True ('!':ss) = removeGarbage' True (tail ss)
        removeGarbage' True ('>':ss) = removeGarbage' False ss
        removeGarbage' True (_:ss) = removeGarbage' True ss
        removeGarbage' False (s:ss) = s:(removeGarbage' False ss)
