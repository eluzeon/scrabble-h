module Game.Types where

import Data.List


data Point 
    = Simple -- Green field
    | DoubleLetter -- Light blue double points for letter
    | DoubleWord -- Pink double points for word
    | TripleLetter -- Blue tripple points for letter
    | TrippleWord -- Red tripple p. for word

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
    deriving (Show, Eq)


sybmolsPoints = [
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
