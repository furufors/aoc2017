#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import qualified Data.Map as M
import Data.Either
import Data.List (sortBy)
import Data.Ord (comparing)
import Text.Parsec
import Text.Parsec.Char

type ThreeD = (Int, Int, Int)
data Particle = Particle
    { pos :: ThreeD
    , vel :: ThreeD
    , acc :: ThreeD
    , index :: Int
    }

main :: IO ()
main = interact $ show . index . head . sortBy (comparing absAccSq) . map (parseParticle) . zip [0..] . lines

absAccSq :: Particle -> Int
absAccSq p = let (a,b,c) = acc p in a * a + b * b + c * c

parseParticle :: (Int,String) -> Particle
parseParticle (i, s) =
    let (a,b,c) = (fromRight (error "Cannot parse program") . parse parseVecs "parseProgram") $ s
    in Particle a b c i

parseVecs :: Parsec String () (ThreeD, ThreeD, ThreeD)
parseVecs = do
    string "p="
    pos <- parseTripple
    string ", v="
    vel <- parseTripple
    string ", a="
    acc <- parseTripple
    return (pos, vel, acc)

parseTripple :: Parsec String () ThreeD
parseTripple = do
    string "<"
    a <- readNum
    string ","
    b <- readNum
    string ","
    c <- readNum
    string ">"
    return (a, b, c)

readNum :: Parsec String () Int
readNum = do
    spaces
    b <- try neg <|> (return False)
    n <- read <$> many digit
    return $ if b then (-1 * n) else n

neg :: Parsec String () Bool
neg = do
    string "-"
    return True
