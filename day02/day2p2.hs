#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Either
import Text.Parsec
import Text.Parsec.Char

main :: IO ()
main = interact $ show . sum .  map (divisible . parseline) . lines

divisible :: [Int] -> Int
divisible is = head [i `div` j | i <- is, j <- is, i /= j, i `mod` j == 0]

parseline :: String -> [Int]
parseline = fromRight [] . parse numbers ""
    where
        numbers = do
            ns <- map read <$> many1 digit `sepBy` spaces
            return ns