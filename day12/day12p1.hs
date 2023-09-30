#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

import Data.Either
import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.Combinator
type Visited = Bool
type PipeSystem = M.Map Int (Visited, [Int])

main :: IO ()
main = interact $ show . runPipes 0 [0] . foldl setup M.empty . lines

runPipes :: Int -> [Int]-> PipeSystem -> Int
runPipes i [    ] ps = i
runPipes i (n:ns) ps =
    let (isVisited, toQueue) = M.findWithDefault (True, []) n ps
    in if isVisited
       then runPipes i ns ps
       else runPipes (i + 1) (ns ++ toQueue) (M.insert n (True,toQueue) ps)

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
