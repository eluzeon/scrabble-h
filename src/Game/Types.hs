{-# LANGUAGE DeriveGeneric #-}

module Game.Types where

import Data.List
import Data.Char (toUpper)
import Data.Aeson
import GHC.Generics

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
    | Any
    deriving (Show, Eq, Ord, Enum, Read, Generic)

instance ToJSON Symbol
instance FromJSON Symbol


data Point 
    = Simple {symbol :: Maybe Symbol } -- Green field
    | DoubleLetter {symbol :: Maybe Symbol} -- Light blue double points for letter
    | DoubleWord {symbol :: Maybe Symbol}-- Pink double points for word
    | TrippleLetter {symbol :: Maybe Symbol}-- Blue tripple points for letter
    | TrippleWord {symbol :: Maybe Symbol} -- Red tripple p. for word
    deriving (Eq, Show, Generic)

instance ToJSON Point
instance FromJSON Point

type Board = [[Point]]

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
    (Z, 10),
    (Any, 0)]

getSymbPoints :: Symbol -> Int
getSymbPoints x = snd $ head $ filter (\(s, _) -> s == x) symbolsPoints

fromString :: [Char] -> Symbol
fromString "ANY" = Any :: Symbol
fromString x = read x :: Symbol

fromIndex :: Int -> Symbol
fromIndex = toEnum

fromIndexList :: [Int] -> [Symbol]
fromIndexList = map fromIndex

toChar :: Symbol -> Char
toChar x = (show x) !!0

-- Get number of points for passed Char
pointsForChar x =  (getSymbPoints . fromString) [toUpper x]

doubleRow :: [a] -> [a]
doubleRow x = (++) x $ tail $ reverse x

tripple = replicate 3 
double = replicate 2
quarduple = replicate 4

emptyBoard :: Board
emptyBoard = doubleRow $ map doubleRow [
    [TrippleWord Nothing, Simple Nothing, Simple Nothing, DoubleLetter Nothing] ++ tripple (Simple Nothing) ++ [TrippleWord Nothing],
    [Simple Nothing, DoubleWord Nothing] ++ (tripple $ Simple Nothing) ++ [TrippleLetter Nothing] ++ (double $ Simple Nothing),
    (double $ Simple Nothing) ++ [DoubleWord Nothing] ++ (tripple $ Simple Nothing) ++ [DoubleLetter Nothing, Simple Nothing],
    [DoubleLetter Nothing] ++ (double $ Simple Nothing) ++ [DoubleWord Nothing] ++ (tripple $ Simple Nothing) ++ [DoubleLetter Nothing],
    (quarduple $ Simple Nothing) ++ [DoubleWord Nothing] ++ (tripple $ Simple Nothing),
    [Simple Nothing, TrippleLetter Nothing] ++ (tripple $ Simple Nothing) ++ [TrippleLetter Nothing] ++ (double $ Simple Nothing),
    (double $ Simple Nothing) ++ [DoubleLetter Nothing] ++ (tripple $ Simple Nothing) ++ [DoubleLetter Nothing, Simple Nothing],
    [TrippleWord Nothing] ++ (double $ Simple Nothing) ++ [DoubleLetter Nothing] ++ (tripple $ Simple Nothing) ++ [Simple Nothing]]

defaultLettersPool :: [String]
defaultLettersPool = (takeLetterNTimes "E" 12) ++ (takeLetterNTimes "A" 9) ++ (takeLetterNTimes "I" 9) ++
    (takeLetterNTimes "O" 8) ++ (takeLetterNTimes "N" 6) ++ (takeLetterNTimes "R" 6) ++
    (takeLetterNTimes "T" 6) ++ (takeLetterNTimes "L" 4) ++ (takeLetterNTimes "S" 4) ++
    (takeLetterNTimes "U" 4) ++ (takeLetterNTimes "D" 4) ++ (takeLetterNTimes "G" 3) ++
    (takeLetterNTimes "B" 2) ++ (takeLetterNTimes "C" 2) ++ (takeLetterNTimes "M" 2) ++
    (takeLetterNTimes "P" 2) ++ (takeLetterNTimes "F" 2) ++ (takeLetterNTimes "H" 2) ++
    (takeLetterNTimes "V" 2) ++ (takeLetterNTimes "W" 2) ++ (takeLetterNTimes "Y" 2) ++
    (takeLetterNTimes "K" 1) ++ (takeLetterNTimes "J" 1) ++ (takeLetterNTimes "X" 1) ++
    (takeLetterNTimes "Q" 1) ++ (takeLetterNTimes "Z" 1) ++ (takeLetterNTimes "ANY" 2)

takeLetterNTimes :: String -> Int -> [String]
takeLetterNTimes letter n = takeLetterNTimes' letter n []

takeLetterNTimes' :: String -> Int -> [String] -> [String]
takeLetterNTimes' _ 0 arr = arr
takeLetterNTimes' letter n arr = takeLetterNTimes' letter (n - 1) (letter : arr)