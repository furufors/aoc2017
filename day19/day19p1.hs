#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Char
data Cell = Empty | Letter Char | Vert | Cross | Horiz deriving (Show, Eq)
data Direction = W | N | E | S deriving (Eq)
type Map = [[Cell]]

data State = State
    { xpos :: Int
    , ypos :: Int
    , dir :: Direction
    , key :: String
    }

main :: IO ()
main = interact $ solve . map (map parseCell) . lines

solve :: Map -> String
solve m = runTubes startState (tile m)

tile :: Map -> State -> Cell
tile m s = if xok && yok
           then m!!(ypos s)!!(xpos s)
           else Empty
    where yok = ypos s < (length m)
          xok = xpos s < (length $ m!!(ypos s))

-- s is current position
runTubes :: State -> (State -> Cell) -> String
runTubes s a = runTubes' s
    where
        runTubes' s = case a s of
            Empty -> if key s == ""
                     then runTubes' . right $ s
                     else key s
            Letter c -> runTubes' . continue . addLetter c $ s
            Vert -> runTubes' . continue $ s
            Horiz -> runTubes' . continue $ s
            Cross -> let above = a (up s)
                         leftOf = a (left s)
                         hasAbove = cellIsLetter above || above == Vert
                         hasLeftOf = cellIsLetter leftOf || leftOf == Horiz
                         vCont = let dir' = if hasAbove then N else S in runTubes' . continue $ s { dir = dir' }
                         hCont = let dir' = if hasLeftOf then W else E in runTubes' . continue $ s { dir = dir' }
                     in if dir s `elem` [W,E] then vCont else hCont

cellIsLetter :: Cell -> Bool
cellIsLetter (Letter _) = True
cellIsLetter _          = False

continue :: State -> State
continue s = case dir s of
    W -> left s
    N -> up s
    E -> right s
    S -> down s

right :: State -> State
right s = let x' = xpos s + 1 in s { xpos = x'}
left s  = let x' = xpos s - 1 in s { xpos = x'}
up s    = let y' = ypos s - 1 in s { ypos = y'}
down s  = let y' = ypos s + 1 in s { ypos = y'}

addLetter :: Char -> State -> State
addLetter c s = let key' = key s ++ [c] in s { key = key' }

startState :: State
startState = State 0 0 S ""

parseCell :: Char -> Cell
parseCell ' ' = Empty
parseCell '|' = Vert
parseCell '-' = Horiz
parseCell '+' = Cross
parseCell c | isLetter c = Letter c
parseCell c = error $ "Error parsing: " ++ [c]
