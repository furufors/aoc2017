#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

import Data.Either
import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.Combinator
type Visited = Bool
type PipeSystem = M.Map Int (Visited, [Int])

main :: IO ()
main = interact $ show . countGroups 0 . foldl setup M.empty . lines

countGroups :: Int -> PipeSystem -> Int
countGroups i ps | allVisited ps = i
countGroups i ps = countGroups (i + 1) (runPipes (firstUnvisited ps) ps)

allVisited :: PipeSystem -> Bool
allVisited = all id . map fst . M.elems

firstUnvisited :: PipeSystem -> [Int]
firstUnvisited = take 1 . M.keys . M.filter (not . fst)

runPipes :: [Int]-> PipeSystem -> PipeSystem
runPipes [    ] ps = ps
runPipes (n:ns) ps =
    let (isVisited, toQueue) = M.findWithDefault (True, []) n ps
    in if isVisited
       then runPipes ns ps
       else runPipes (ns ++ toQueue) (M.insert n (True,toQueue) ps)

setup :: PipeSystem -> String -> PipeSystem
setup ps ss =
    let (i, is) = fromRight (error "Failed to parse") . parse parsePipe "parsePipe" $ ss
    in M.insert i (False, is) ps

parsePipe :: Parsec String () (Int, [Int])
parsePipe = do
    i <- read <$> many1 digit
    string " <-> "
    is <- map read <$> (many1 digit) `sepBy` (string ", ")
    return (i,is)
