module Game.Types where

import Data.List
import Data.Char (toUpper)

-- Symbols which can be set into field point
data Symbol 
    = A
    | B
    | C
    | D 
    | E
    | F
    | G 
    | H 
    | I
    | J 
    | K
    | L 
    | M 
    | N 
    | O 
    | P
    | Q
    | R
    | S
    | T
    | U
    | V
    | W
    | X
    | Y
    | Z
    deriving (Show, Eq, Ord, Enum, Read)


data Point 
    = Simple {symbol :: Maybe Symbol } -- Green field
    | DoubleLetter {symbol :: Maybe Symbol} -- Light blue double points for letter
    | DoubleWord {symbol :: Maybe Symbol}-- Pink double points for word
    | TrippleLetter {symbol :: Maybe Symbol}-- Blue tripple points for letter
    | TrippleWord {symbol :: Maybe Symbol} -- Red tripple p. for word
    deriving (Show)

type Board = [Point]

symbolsPoints = [
    (A, 1),
    (B, 3),
    (C, 3),
    (D, 2),
    (E, 1),
    (F, 4),
    (G, 2),
    (H, 4),
    (I, 1),
    (J, 8),
    (K, 5),
    (L, 1),
    (M, 3),
    (N, 1),
    (O, 1),
    (P, 3),
    (Q, 10),
    (R, 1),
    (S, 1),
    (T, 1),
    (U, 1),
    (V, 4),
    (W, 4),
    (X, 8),
    (Y, 4),
    (Z, 10)]    

data Player = First | Second

data GameState = Game { board :: Board, player :: Player }

getSymbPoints :: Symbol -> Int
getSymbPoints x = snd $ head $ filter (\(s, _) -> s == x) symbolsPoints

fromString :: [Char] -> Symbol
fromString x = read x :: Symbol

-- Get number of points for passed Char
pointsForChar x =  (getSymbPoints . fromString) [toUpper x]

doubleRow :: [a] -> [a]
doubleRow x = (++) x $ tail $ reverse x

tripple = replicate 3 
double = replicate 2
quarduple = replicate 4

emptyBoard = doubleRow $ map doubleRow [
    [TrippleWord Nothing, Simple Nothing, Simple Nothing, DoubleLetter Nothing] ++ tripple (Simple Nothing) ++ [TrippleWord Nothing],
    [Simple Nothing, DoubleWord Nothing] ++ (tripple $ Simple Nothing) ++ [TrippleLetter Nothing] ++ (double $ Simple Nothing),
    (double $ Simple Nothing) ++ [DoubleWord Nothing] ++ (tripple $ Simple Nothing) ++ [DoubleLetter Nothing, Simple Nothing],
    [DoubleLetter Nothing] ++ (double $ Simple Nothing) ++ [DoubleWord Nothing] ++ (tripple $ Simple Nothing) ++ [DoubleLetter Nothing],
    (quarduple $ Simple Nothing) ++ [DoubleWord Nothing] ++ (tripple $ Simple Nothing),
    [Simple Nothing, TrippleLetter Nothing] ++ (tripple $ Simple Nothing) ++ [TrippleLetter Nothing] ++ (double $ Simple Nothing),
    (double $ Simple Nothing) ++ [DoubleLetter Nothing] ++ (tripple $ Simple Nothing) ++ [DoubleLetter Nothing, Simple Nothing],
    [TrippleWord Nothing] ++ (double $ Simple Nothing) ++ [DoubleLetter Nothing] ++ (tripple $ Simple Nothing) ++ [DoubleWord Nothing]]