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
                jmps' = if offset >= 3
                        then modify (\x -> x - 1) ptr jmps
                        else modify (\x -> x + 1) ptr jmps
                count' = count + 1
            --in trace (show jmps ++ " : " ++ show ptr) $ runMaze' count' ptr' jmps'
            in runMaze' count' ptr' jmps'

modify :: (Int -> Int) -> Int -> [Int] -> [Int]
modify f _ [    ] = error "Reached end of list"
modify f 0 (i:is) = (f i):is
modify f n (i:is) = i:(modify f (n-1) is)