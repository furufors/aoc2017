#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List (nub)
main :: IO ()
main = interact $ show . length . filter valid . lines

valid :: String -> Bool
valid s = let ss = words s
          in length ss == (length . nub $ ss)