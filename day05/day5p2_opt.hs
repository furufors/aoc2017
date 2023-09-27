#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Vector.Unboxed ((!),Vector(..))
import qualified Data.Vector.Unboxed as VU
main :: IO ()
main = interact $ show . runMaze . parse . lines

parse :: [String] -> Vector Int
parse = VU.fromList . map read

runMaze :: Vector Int -> Int
runMaze jmps = runMaze' 0 0 jmps
    where
        runMaze' count ptr jmps | ptr < 0 = count
        runMaze' count ptr jmps | ptr > (VU.length jmps - 1) = count
        runMaze' count ptr jmps =
            let offset = jmps!ptr
                ptr' = ptr + offset
                jmps' = if offset >= 3
                        then modify (\x -> x - 1) ptr jmps
                        else modify (\x -> x + 1) ptr jmps
                count' = count + 1
            --in trace (show jmps ++ " : " ++ show ptr) $ runMaze' count' ptr' jmps'
            in runMaze' count' ptr' jmps'

modify :: (Int -> Int) -> Int -> Vector Int -> Vector Int
modify f i v = let x = v!i
                   modification = VU.singleton (i, f x)
                 in VU.update v modification