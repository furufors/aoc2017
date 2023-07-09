#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Text.Parsec
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Debug.Trace
type Node = (String, (Int,Maybe Int))
type References = [String]
type Mapping = Map String ((Int,Maybe Int),References)

main :: IO ()
main = interact $ show . balance . calculateWeights . buildTree . map parsein . lines

balance :: Mapping -> [Int]
balance m =
    let start = findRoot m
    in checkImbalance start m

checkImbalance :: String -> Mapping -> [Int]
checkImbalance s m =  case M.lookup s m of
    Nothing -> error "Missing node"
    Just ((i,Nothing),rs) -> error "Uncalculated node"
    Just ((i,Just n),rs) ->
        let weights = map (lookupWeight m) rs
        in if not (null weights ) && and (map (== head weights) (tail weights))
           then concat . map (\s -> checkImbalance s m) $ rs
           else
                let target = (sort weights)!!1
                in filter (/= 0) $ map (differenceToTarget target m) rs

differenceToTarget :: Int -> Mapping -> String -> Int
differenceToTarget t m s = case M.lookup s m of
    Nothing -> 0
    Just ((i,Nothing),rs) -> error "Uncalculated node"
    Just ((i,Just n),rs) -> if n == t
                            then 0
                            else i - (n-t)

calculateWeights :: Mapping -> Mapping
calculateWeights m =
    let rootNode = findRoot m
    in calculateWeight m rootNode

calculateWeight :: Mapping -> String -> Mapping
calculateWeight m s = case M.lookup s m of
    Nothing -> error "Missing node"
    Just ((i,Just n),rs) -> m
    Just ((i,Nothing),rs) ->
        let m' = foldl' calculateWeight m rs
            n  = sumWeights rs m'
            sumWeights :: [String] -> Mapping -> Int
            sumWeights rs m = sum $ map (lookupWeight m) rs
        in M.insert s ((i,Just $ i + n),rs) m'

lookupWeight :: Mapping -> String -> Int
lookupWeight m s = case M.lookup s m >>= snd . fst of
    Just n -> n
    Nothing -> error "Uncalculated node"

findRoot :: Mapping -> String
findRoot m = stepUp . head $ M.keys m
    where
        stepUp k =
            let filtered = filter (\(_,(_,rs)) -> k `elem` rs) $ M.assocs m
            in if null filtered
               then k
               else stepUp . fst . head $ filtered

buildTree :: [(Node,References)] -> Mapping
buildTree ns = foldl step M.empty ns
    where
        step :: Mapping -> (Node,References) -> Mapping
        step m ((n,i),l) = M.insert n (i,l) m

parsein :: String -> (Node,References)
parsein input = case parse (try parseBranch <|> try parseLeaf) "parsein" input of
    Left err -> error $ show err
    Right a -> a

parseBranch :: Parsec String () (Node,References)
parseBranch = do
    n <- many1 lower
    _ <- string " ("
    i <- read <$> many1 digit
    _ <- string ") -> "
    l <- (many1 lower) `sepBy` (string ", ")
    return $ ((n,(i, Nothing)),l)

parseLeaf :: Parsec String () (Node,References)
parseLeaf = do
    n <- many1 lower
    _ <- string " ("
    i <- read <$> many1 digit
    _ <- string ")"
    return $ ((n,(i, Just i)),[])