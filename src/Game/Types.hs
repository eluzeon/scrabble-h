module Game.Types where

import Data.List

-- Point is Free (if Int) or Filled (with Sybmol)
type Point = Either Int Symbol
type Board = [Point]

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
    | R
    | S
    | T
    | U
    | V
    | W
    | X
    | Y
    | Z
    deriving (Show, Eq)

data Player = First | Second
data GameState = Game { board :: Board, player :: Player }
