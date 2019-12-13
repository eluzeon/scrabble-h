module Extensions where

import Control.Monad.IO.Class
import System.Random
import WebApiTypes
import Game.Types
import Control.Lens


-- from, to, letters, n, IO result
returnNLettersFromGetLetters :: Int -> Int -> [String] -> Int -> IO TakeNLettersDto
returnNLettersFromGetLetters from to letters n = returnNLettersFromGetLetters' from to letters n []

-- from, to, letters, n, resultArray, IO result
returnNLettersFromGetLetters' :: Int -> Int -> [String] -> Int -> [String] -> IO TakeNLettersDto
returnNLettersFromGetLetters' _ _ [] _ arr = return $ TakeNLettersDto [] arr
returnNLettersFromGetLetters' _ _ letters 0 arr = return $ TakeNLettersDto letters arr
returnNLettersFromGetLetters' from to letters n arr = do
  index <- liftIO $ getRandomIndex from to
  returnNLettersFromGetLetters' from to ((getBeforeIndex index letters) ++ (getAfterIndex index letters)) (n - 1) (letters !! index : arr)

getRandomIndex :: Int -> Int -> IO Int
getRandomIndex from to = do
        g <- newStdGen
        randomRIO (from, to)

getBeforeIndex :: Int -> [String] -> [String]
getBeforeIndex index letters = getBeforeIndex' index letters []

getBeforeIndex' :: Int -> [String] -> [String] -> [String]
getBeforeIndex' 0 _ result = result
getBeforeIndex' index letters result = getBeforeIndex' (index - 1) letters ((letters !! (index - 1)) : result)

getAfterIndex :: Int -> [String] -> [String]
getAfterIndex index letters = getAfterIndex' index letters []

getAfterIndex' :: Int -> [String] -> [String] -> [String]
getAfterIndex' index letters result 
  | index == ((length letters) - 1) = result
  | otherwise = getAfterIndex' (index + 1) letters ((letters !! (index + 1)) : result)
 


getNextPlayer :: Int -> Int -> Int
getNextPlayer playerTurnNumber numberOfPlayers
    | playerTurnNumber == numberOfPlayers = 1
    | otherwise = playerTurnNumber + 1

data TakeNLettersDto
  = TakeNLettersDto {
  remainingLetters :: [String],
  lettersToReturn :: [String]
}


getPointData :: Changes -> Board -> Point
getPointData ch board = case oldPoint of
  Simple _ -> Simple $ Just $ fromString $ letter ch
  DoubleLetter _ -> DoubleLetter $ Just $ fromString $ letter ch
  DoubleWord _ -> DoubleWord $ Just $ fromString $ letter ch
  TrippleLetter _ -> TrippleLetter $ Just $ fromString $ letter ch
  TrippleWord _ -> TrippleWord $ Just $ fromString $ letter ch
  where oldPoint = (board !! (positionY ch) !! (positionX ch))


applyForChange :: Changes -> Board -> Board
applyForChange ch board = board & element (positionY ch) . element (positionX ch) .~ (getPointData ch board)


applyChanges :: [Changes] -> Board -> Board
applyChanges changes initBoard = foldr (\x brd -> applyForChange x brd) initBoard changes


fromChanges :: [Changes] -> [(Int, Int)]
fromChanges changes = map (\x -> (positionX x, positionY x)) changes
