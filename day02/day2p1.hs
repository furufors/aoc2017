#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Either
import Text.Parsec
import Text.Parsec.Char

main :: IO ()
main = interact $ show . sum .  map (maxMinDiff . parseline) . lines

maxMinDiff :: [Int] -> Int
maxMinDiff is = let big = maximum is
                    small = minimum is
                in big - small

parseline :: String -> [Int]
parseline = fromRight [] . parse numbers ""
    where
        numbers = do
            ns <- map read <$> many1 digit `sepBy` spaces
            return ns