#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import qualified Data.Map as M
import Data.Either
import Text.Parsec
import Text.Parsec.Char

data Ref = Lit Int | Reg Char deriving Show
data Instruction = Snd Ref
                 | Set Ref Ref
                 | Add Ref Ref
                 | Mul Ref Ref
                 | Mod Ref Ref
                 | Rcv Ref
                 | Jgz Ref Ref deriving Show
type Program = [Instruction]
type Registers = M.Map Char Int
type Pointer = Int

-- snd X plays a sound with a frequency equal to the value of X.
-- set X Y sets register X to the value of Y.
-- add X Y increases register X by the value of Y.
-- mul X Y sets register X to the result of multiplying the value contained in register X by the value of Y.
-- mod X Y sets register X to the remainder of dividing the value contained in register X by the value of Y (that is, it sets X to the result of X modulo Y).
-- rcv X recovers the frequency of the last sound played, but only when the value of X is not zero. (If it is zero, the command does nothing.)
-- jgz X Y jumps with an offset of the value of Y, but only if the value of X is greater than zero. (An offset of 2 skips the next instruction, an offset of -1 jumps to the previous instruction, and so on.)

main :: IO ()
main = interact $ show . runProgram . (\a -> (False, a, M.empty, 0)) . parseProgram . lines

runProgram :: (Bool, Program, Registers, Pointer) -> Int
runProgram (False, is, rs, pt) = runProgram $ runInstruction (is!!pt) (is, rs, pt)
runProgram (True, is, rs, pt) =
    case M.lookup '~' rs of
        Just i    -> i
        otherwise -> error "No sound played yet"

runInstruction :: Instruction -> (Program, Registers, Pointer) -> (Bool, Program, Registers, Pointer)
runInstruction (Snd x) (is, rs, pt) = (False, is, M.insert '~' (getVal rs x) rs, pt + 1)
runInstruction (Set (Reg c) y) (is, rs, pt) = (False, is, M.insert c (getVal rs y) rs, pt +1)
runInstruction (Set (Lit i) y) (is, rs, pt) = error "Set cannot take a litteral first argument"
runInstruction (Add (Reg c) y) (is, rs, pt) =
    let v0 = M.findWithDefault 0 c rs
        v1 = v0 + getVal rs y
    in (False, is, M.insert c v1 rs, pt + 1)
runInstruction (Add (Lit i) y) (is, rs, pt) = error "Add cannot take a litteral first argument"
runInstruction (Mul (Reg c) y) (is, rs, pt) =
    let v0 = M.findWithDefault 0 c rs
        v1 = v0 * getVal rs y
    in (False, is, M.insert c v1 rs, pt + 1)
runInstruction (Mul (Lit i) y) (is, rs, pt) = error "Mul cannot take a litteral first argument"
runInstruction (Mod (Reg c) y) (is, rs, pt) =
    let v0 = M.findWithDefault 0 c rs
        v1 = v0 `mod` getVal rs y
    in (False, is, M.insert c v1 rs, pt + 1)
runInstruction (Mod (Lit i) y) (is, rs, pt) = error "Mod cannot take a litteral first argument"
runInstruction (Rcv x) (is, rs, pt) =
    if (getVal rs x) == 0
    then (False, is, rs, pt + 1)
    else (True, is, rs, pt)
runInstruction (Jgz x y) (is, rs, pt) =
    if getVal rs x > 0
    then (False, is, rs, pt + (getVal rs y))
    else (False, is, rs, pt + 1)


getVal :: Registers -> Ref -> Int
getVal rs (Lit i) = i
getVal rs (Reg c) =  M.findWithDefault 0 c rs

parseProgram :: [String] -> Program
parseProgram = map (fromRight (error "Cannot parse program") . parse parseInstruction "parseProgram")

parseInstruction :: Parsec String () Instruction
parseInstruction = try parseSnd
               <|> try parseSet
               <|> try parseAdd
               <|> try parseMul
               <|> try parseMod
               <|> try parseRcv
               <|> try parseJgz

parseRef :: Parsec String () Ref
parseRef = try parseLit <|> try parseNegLit <|> try parseReg

parseLit :: Parsec String () Ref
parseLit = do
    i <- read <$> many1 digit
    return (Lit i)

parseNegLit :: Parsec String () Ref
parseNegLit = do
    _ <- char '-'
    i <- ((-1)*) . read <$> many1 digit
    return (Lit i)

parseReg :: Parsec String () Ref
parseReg = do
    c <- letter
    return (Reg c)

parseSnd :: Parsec String () Instruction
parseSnd = do
    _  <- string "snd "
    r1 <- parseRef
    return (Snd r1)

parseSet :: Parsec String () Instruction
parseSet = do
    _  <- string "set "
    r1 <- parseRef
    _  <- string " "
    r2 <- parseRef
    return (Set r1 r2)

parseAdd :: Parsec String () Instruction
parseAdd = do
    _  <- string "add "
    r1 <- parseRef
    _  <- string " "
    r2 <- parseRef
    return (Add r1 r2)

parseMul :: Parsec String () Instruction
parseMul =  do
    _  <- string "mul "
    r1 <- parseRef
    _  <- string " "
    r2 <- parseRef
    return (Mul r1 r2)

parseMod :: Parsec String () Instruction
parseMod =  do
    _  <- string "mod "
    r1 <- parseRef
    _  <- string " "
    r2 <- parseRef
    return (Mod r1 r2)

parseRcv :: Parsec String () Instruction
parseRcv = do
    _  <- string "rcv "
    r1 <- parseRef
    return (Rcv r1)

parseJgz :: Parsec String () Instruction
parseJgz = do
    _  <- string "jgz "
    r1 <- parseRef
    _  <- string " "
    r2 <- parseRef
    return (Jgz r1 r2)
