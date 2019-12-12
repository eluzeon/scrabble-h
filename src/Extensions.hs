module Extensions where

import Control.Monad.IO.Class
import System.Random

-- from, to, letters, n, IO result
returnNLettersFromGetLetters :: Int -> Int -> [String] -> Int -> IO [String]
returnNLettersFromGetLetters from to letters n = returnNLettersFromGetLetters' from to letters n []

-- from, to, letters, n, resultArray, IO result
returnNLettersFromGetLetters' :: Int -> Int -> [String] -> Int -> [String] -> IO [String]
returnNLettersFromGetLetters' _ _ [] _ arr = return arr
returnNLettersFromGetLetters' _ _ _ 0 arr = return arr
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