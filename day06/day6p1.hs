#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List
type Memory = [Int]

main :: IO ()
main = interact $ show . findCycles . parse . lines . tabToNewline

findCycles :: Memory -> Int
findCycles m =
    let findCycles' i hist m =
            let m' = reallocate m
            in if m' `elem` hist
            then i + 1
            else findCycles' (i+1) (m':hist) m'
    in findCycles' 0 [] m

reallocate :: Memory -> Memory
reallocate m =
    let memoryWithIndex = zip [0..] m
        (i, blocks) = head $ sortBy maximumBlockOrLowestIndex memoryWithIndex
        swapOut = take i m ++ [0] ++ drop (i+1) m
    in distribute blocks (i+1) swapOut

distribute :: Int -> Int -> Memory -> Memory
distribute b i m =
    let maxindex = length m - 1
        distribute' 0 _ m = m
        distribute' b i m = if i > maxindex
                            then distribute' b 0 m
                            else distribute' (b-1) (i+1) $ increment i m
        increment _ [    ] = error "Reached end of list!"
        increment 0 (e:es) = (e+1):es
        increment n (e:es) = e:(increment (n-1) es)
    in distribute' b i m

maximumBlockOrLowestIndex :: (Int,Int) -> (Int,Int) -> Ordering
maximumBlockOrLowestIndex (i1,b1) (i2,b2) = case compare b1 b2 of
    GT -> LT
    LT -> GT
    EQ -> compare i1 i2

parse :: [String] -> Memory
parse = map read

tabToNewline :: String -> String
tabToNewline [] = []
tabToNewline ('\t':cs) = '\n':(tabToNewline cs)
tabToNewline (c:cs) = c:(tabToNewline cs)