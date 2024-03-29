#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

import Data.Either
import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.Char

type Regs = M.Map String Int
type Inst = Int -> Int -> Int
type Cond = Int -> Int -> Bool
type Rule = (String, Inst, Int, String, Cond, Int)

main :: IO ()
main = interact $ show . maximum . M.elems . foldl runRule M.empty . parseRules . lines

runRule :: Regs -> Rule -> Regs
runRule rs (r1, inst, val1, r2, cond, val2) =
    let rv1 = M.findWithDefault 0 r1 rs
        rv2 = M.findWithDefault 0 r2 rs
        rv1' = inst rv1 val1
    in if cond rv2 val2
       then M.insert r1 rv1' rs
       else rs

parseRules :: [String] -> [Rule]
parseRules = map (fromRight (error "Cannot parse rule-set") . parse parseRule "parseRule")

parseRule :: Parsec String () Rule
parseRule = do
    r1 <- many1 letter
    inst <- parseInst
    val1 <- readNum
    string " if "
    r2 <- many1 letter
    cond <- parseCond
    val2 <- readNum
    return (r1, inst, val1, r2, cond, val2)

parseInst :: Parsec String () Inst
parseInst = try (string " inc " >> return (+))
        <|> try (string " dec " >> return (-))

parseCond :: Parsec String () Cond
parseCond = try (string " < " >> return (<))
        <|> try (string " <= " >> return (<=))
        <|> try (string " > " >> return (>))
        <|> try (string " >= " >> return (>=))
        <|> try (string " == " >> return (==))
        <|> try (string " != " >> return (/=))

readNum :: Parsec String () Int
readNum = do
    b <- try (string "-" >> return True) <|> (return False)
    n <- read <$> many digit
    return $ if b then (-1 * n) else n
