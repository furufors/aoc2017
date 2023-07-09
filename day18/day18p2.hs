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
type Queue = ([Int],[Int])
type MachState = (Program, Registers, Pointer)

-- snd X plays a sound with a frequency equal to the value of X.
-- set X Y sets register X to the value of Y.
-- add X Y increases register X by the value of Y.
-- mul X Y sets register X to the result of multiplying the value contained in register X by the value of Y.
-- mod X Y sets register X to the remainder of dividing the value contained in register X by the value of Y (that is, it sets X to the result of X modulo Y).
-- rcv X recovers the frequency of the last sound played, but only when the value of X is not zero. (If it is zero, the command does nothing.)
-- jgz X Y jumps with an offset of the value of Y, but only if the value of X is greater than zero. (An offset of 2 skips the next instruction, an offset of -1 jumps to the previous instruction, and so on.)

setupMachines :: Program -> ((Queue, MachState),(Queue, MachState))
setupMachines is =
    let s0 = M.insert 'p' 0 M.empty
        s1 = M.insert 'p' 1 M.empty
        m0 = (([],[]), (is,s0,0))
        m1 = (([],[]), (is,s1,0))
        i = is!!0
    in (runInstruction i m0, runInstruction i m1)

main :: IO ()
main = interact $ show . runPrograms . setupMachines . parseProgram . lines

runPrograms :: ((Queue, MachState),(Queue, MachState)) -> Int
-- Base case, we're done
runPrograms ((([],[]),(_,rs0,_)),(([],[]),(_,rs1,_)))=
    case M.lookup '~' rs1 of
        Just i    -> i
        otherwise -> error "Program 1 hasn't sent any values yet"
runPrograms (((qin0,qout0),(is0,rs0,pt0)),((qin1,qout1),(is1,rs1,pt1))) =
    let i0 = is0!!pt0
        i1 = is1!!pt1
        m0 = ((qin0 ++ qout1, []), (is0,rs0,pt0))
        m1 = ((qin1 ++ qout0, []), (is1,rs1,pt1))
    in runPrograms (runInstruction i0 m0, runInstruction i1 m1)


runInstruction :: Instruction -> (Queue, MachState) -> (Queue, MachState)
runInstruction (Snd x) ((qin, qout), (is, rs, pt)) =
    let cnt = M.findWithDefault 0 '~' rs
        rs' = M.insert '~' (cnt + 1) rs
    in runInstruction (is!!(pt + 1)) $ ((qin, qout ++ [getVal rs x]), (is, rs', pt + 1))
runInstruction (Set (Reg c) y) ((qin, qout), (is, rs, pt)) =
    runInstruction (is!!(pt + 1)) $ ((qin,qout), (is, M.insert c (getVal rs y) rs, pt + 1))
runInstruction (Set (Lit i) y) ((qin, qout), (is, rs, pt)) =
    error "Set cannot take a litteral first argument"
runInstruction (Add (Reg c) y) ((qin, qout), (is, rs, pt)) =
    let v0 = M.findWithDefault 0 c rs
        v1 = v0 + getVal rs y
    in runInstruction (is!!(pt + 1)) $ ((qin, qout), (is, M.insert c v1 rs, pt + 1))
runInstruction (Add (Lit i) y) ((qin, qout), (is, rs, pt)) =
    error "Add cannot take a litteral first argument"
runInstruction (Mul (Reg c) y) ((qin, qout), (is, rs, pt)) =
    let v0 = M.findWithDefault 0 c rs
        v1 = v0 * getVal rs y
    in runInstruction (is!!(pt + 1)) $ ((qin, qout), (is, M.insert c v1 rs, pt + 1))
runInstruction (Mul (Lit i) y) ((qin, qout), (is, rs, pt)) =
    error "Mul cannot take a litteral first argument"
runInstruction (Mod (Reg c) y) ((qin, qout), (is, rs, pt)) =
    let v0 = M.findWithDefault 0 c rs
        v1 = v0 `mod` getVal rs y
    in runInstruction (is!!(pt + 1)) $ ((qin, qout), (is, M.insert c v1 rs, pt + 1))
runInstruction (Mod (Lit i) y) ((qin, qout), (is, rs, pt)) =
    error "Mod cannot take a litteral first argument"
runInstruction (Rcv x) (([], qout), (is, rs, pt)) =
    (([], qout), (is, rs, pt))
runInstruction (Rcv (Reg c)) (((qin:qins), qout), (is, rs, pt)) =
    let rs' = M.insert c qin rs
    in runInstruction (is!!(pt + 1)) $ ((qins, qout), (is, rs', pt + 1))
runInstruction (Jgz x y) ((qin, qout), (is, rs, pt)) =
    if getVal rs x > 0
    then runInstruction (is!!(pt + (getVal rs y))) $ ((qin, qout), (is, rs, pt + (getVal rs y)))
    else runInstruction (is!!(pt + 1)) $ ((qin, qout), (is, rs, pt + 1))


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
