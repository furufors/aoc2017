#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

import Data.Either
import qualified Data.Map as M
import Data.Maybe
import Text.Parsec
import Text.Parsec.Char

type Move = DanceFloor -> DanceFloor
type Moves = [Move]
type DanceFloor = M.Map Int Char

-- Running the program one billion times is unfeasible.
-- We instead search for a point where the DanceFloor starts to repeat in patterns.
-- At this point we can infer what the position after one billion iterations by:
-- 1000000000 = kilo

main :: IO ()
main = interact $ findFixpoint dance startFloor [] . parseMoves . head . lines

floorSize = 16

findFixpoint :: (DanceFloor -> Moves -> DanceFloor) -> DanceFloor -> [(String,Int)] -> Moves -> String
findFixpoint f df lg mvs =
    let df' = f df mvs
        dfs' = dfToString df'
        fixpointFound = any (== dfs') $ map fst lg
        repeatAt = length lg
        firstOccurrance = snd . head $ filter (\x -> (fst x ) == dfs') lg
        indexAtBillion = (1000000000 - firstOccurrance + 1) `mod` repeatAt
        posAtBillion = fst . head $ filter (\x -> (snd x ) == indexAtBillion) lg
    in if fixpointFound
       then "Fixpoint cycle: " ++ show (repeatAt - firstOccurrance)
            ++ ", first occurrance: " ++ show firstOccurrance
            ++ ", dancefloor: " ++ posAtBillion
       else findFixpoint f df' ((dfs', length lg + 1):lg) mvs

dfToString :: DanceFloor -> String
dfToString = map snd . M.toList

dance :: DanceFloor -> Moves -> DanceFloor
dance df [] = df
dance df (a:as) = dance (a df) as

startFloor :: DanceFloor
startFloor = M.fromList $ zip (take floorSize [0..]) (take floorSize ['a'..])

parseMoves :: String -> Moves
parseMoves = (fromRight (error "error parsing")) . (parse parseMove "parseMove")

parseMove :: Parsec String () Moves
parseMove = possibilities `sepBy` (string ",")
    where
        possibilities = try parseSpin
                    <|> try parseExchange
                    <|> try parsePartner
        parseSpin = do
            string "s"
            count <- read <$> many1 digit
            return (spin count)
        parseExchange = do
            string "x"
            i1 <- read <$> many1 digit
            string "/"
            i2 <- read <$> many1 digit
            return (exchange i1 i2)
        parsePartner = do
            string "p"
            p1 <- lower
            string "/"
            p2 <- lower
            return (partner p1 p2)

spin :: Int -> DanceFloor -> DanceFloor
spin i df = M.mapKeys (\k -> (k + i) `mod` floorSize) df

exchange :: Int -> Int -> DanceFloor -> DanceFloor
exchange ai bi df =
    let pa = fromJust $ M.lookup ai df
        pb = fromJust $ M.lookup bi df
    in M.insert ai pb $ M.insert bi pa df

partner :: Char -> Char -> DanceFloor -> DanceFloor
partner a b df =
    let ai = positionOf a df
        bi = positionOf b df
    in exchange ai bi df

positionOf :: Char -> DanceFloor -> Int
positionOf a df =
    let matches = M.keys $ M.filter (== a) df
    in if length matches == 1
       then head matches
       else error $ "Missing value: " ++ [a]
