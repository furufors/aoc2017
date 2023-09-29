#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import qualified Data.Map as M
import Data.Either
import Data.List (sortBy, groupBy)
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
main = interact $ show . runPhysics . removeColliding . map (parseParticle) . zip [0..] . lines

runPhysics :: [Particle] -> Int
runPhysics ps =
    if convergence ps
    then length ps
    else runPhysics $ step ps
    where
        step = removeColliding . map update
        update p = let (ax,ay,az) = acc p
                       (vx,vy,vz) = vel p
                       (px,py,pz) = pos p
                   in p { vel = (vx+ax,vy+ay,vz+az), pos = (px+vx+ax,py+vy+ay,pz+vz+az) }
        convergence = all (\p -> posVelAngle p > 0.99999)
        posVelAngle :: Particle -> Double
        posVelAngle p = let (vx,vy,vz) = vel p
                            (px,py,pz) = pos p
                            dotp = fromIntegral $ vx * px + vy * py + vz * pz
                            abspv = sqrt . fromIntegral $ vx * vx + vy * vy + vz * vz
                            abspp = sqrt . fromIntegral $ px * px + py * py + pz * pz
                            absp = abspv * abspp
                        in dotp / absp

removeColliding :: [Particle] -> [Particle]
removeColliding = concat . filter (\l -> length l == 1) . groupBy (\a b -> pos a == pos b)

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
