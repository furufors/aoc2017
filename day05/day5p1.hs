#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Debug.Trace
main :: IO ()
main = interact $ show . runMaze . parse . lines

parse :: [String] -> [Int]
parse = map read

runMaze :: [Int] -> Int
runMaze jmps = runMaze' 0 0 jmps
    where
        runMaze' count ptr jmps | ptr < 0 = count
        runMaze' count ptr jmps | ptr > (length jmps - 1) = count
        runMaze' count ptr jmps =
            let offset = jmps!!ptr
                ptr' = ptr + offset
                jmps' = increment ptr jmps
                count' = count + 1
            --in trace (show jmps ++ " : " ++ show ptr) $ runMaze' count' ptr' jmps'
            in runMaze' count' ptr' jmps'

increment :: Int -> [Int] -> [Int]
increment _ [    ] = error "Reached end of list"
increment 0 (i:is) = (i+1):is
increment n (i:is) = i:(increment (n-1) is)