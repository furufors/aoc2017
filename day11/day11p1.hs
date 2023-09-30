#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

import Data.List.Split (splitOn)
import qualified Data.Map as M
data HexDir = N | NE | SE | S | SW | NW deriving (Eq, Ord, Show)
type HexMap = M.Map HexDir Int

main :: IO ()
main = interact $ show . steps . converge symmetries . parse . splitOn "," . head . lines

steps :: HexMap -> Int
steps = foldl1 (+) . M.elems

converge :: Eq a => (a -> a) -> a -> a
converge f as =
    let as' = f as
    in if as' == as
       then as
       else converge f as'

parse :: [String] -> HexMap
parse = parse' M.empty
    where
        parse' m [] = m
        parse' m ("n":ss) = parse' (increment N m) ss
        parse' m ("ne":ss) = parse' (increment NE m) ss
        parse' m ("se":ss) = parse' (increment SE m) ss
        parse' m ("s":ss) = parse' (increment S m) ss
        parse' m ("sw":ss) = parse' (increment SW m) ss
        parse' m ("nw":ss) = parse' (increment NW m) ss
        parse' m (s:ss) = error $ "Cannot parse: " ++ s
        increment :: HexDir -> HexMap -> HexMap
        increment a m = let aCount = M.findWithDefault 0 a m in M.insert a (aCount + 1) m

symmetries :: HexMap -> HexMap
symmetries = remove N S
           . remove SE NW
           . remove SW NE
           . replace NW NE N
           . replace NE NW N
           . replace N SE NE
           . replace SE N NE
           . replace NE S SE
           . replace S NE SE --   \ n  /
           . replace SE SW S -- nw +--+ ne
           . replace SW SE S --   /    \
           . replace S NW SW -- -+      +-
           . replace NW S SW --   \    /
           . replace SW N NW -- sw +--+ se
           . replace N SW NW --   / s  \

remove :: HexDir -> HexDir -> HexMap -> HexMap
remove a b m =
    let aCount = M.findWithDefault 0 a m
        bCount = M.findWithDefault 0 b m
    in if aCount > 0 && bCount > 0
       then M.insert a (aCount - 1) . M.insert b (bCount - 1) $ m
       else m

replace :: HexDir -> HexDir -> HexDir -> HexMap -> HexMap
replace a b c m =
    let aCount = M.findWithDefault 0 a m
        bCount = M.findWithDefault 0 b m
        cCount = M.findWithDefault 0 c m
    in if aCount > 0 && bCount > 0
       then M.insert a (aCount - 1) . M.insert b (bCount - 1) . M.insert c (cCount + 1) $ m
       else m
