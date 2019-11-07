module Game.State where

import Game.Types

initGame :: GameState
initGame = Game (map Left [x | x <- [0..254]]) First


availablePoints :: Board -> [Int]
availablePoints board = [x | Left x <- board]