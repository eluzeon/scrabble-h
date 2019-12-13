module Game.Logic where


import Game.Types
import Data.List



getRowPairs :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
getRowPairs from to = 
    if fst from == fst to
        then [(i, j) | i <- [fst from], j <- [snd from..snd to]]
        else [(i, j) | i <- [snd from], j <- [fst from..fst to]]


-- Get Point instance from Board by coords
getPoint :: (Int, Int) -> Board -> Point
getPoint (x, y) board = (board!!x)!!y


letterMultipliers :: [Point] -> [Int -> Int]
letterMultipliers points = map getOperator points
        where getOperator (Simple (Just x)) =  (+ (getSymbPoints x))
              getOperator (Simple Nothing) = (+ 0)
              getOperator (DoubleLetter (Just x)) = (+ (2 * (getSymbPoints x)))
              getOperator (DoubleLetter Nothing) = (+ 0)
              getOperator (TrippleLetter (Just x )) = (+ (3 * (getSymbPoints x)))
              getOperator (TrippleLetter Nothing) = (+ 0)
              getOperator (DoubleWord (Just x)) = (+ (getSymbPoints x))
              getOperator (DoubleWord Nothing) = (+ 0)
              getOperator (TrippleWord (Just x)) = (+ (getSymbPoints x))
              getOperator (TrippleWord Nothing) = (+ 0)

wordMultipliers :: [Point] -> [Int -> Int] 
wordMultipliers points = map getOperator $ filter fltP points
        where fltP x = case x of 
                DoubleWord x -> True
                TrippleWord x ->  True
                otherwise -> False

              getOperator (DoubleWord (Just x)) = (* 2)
              getOperator (DoubleWord Nothing) = (* 1)
              getOperator (TrippleWord (Just x)) = (* 3)
              getOperator (TrippleWord Nothing) = (* 1)


calculatePointsFromList :: [(Int, Int)] -> Board -> Int
calculatePointsFromList coords board = foldr (\_x _acc -> _x _acc) (foldr (\x acc -> x acc) 0 $ letterMultipliers points) $ wordMultipliers points
        where points = map (\x -> getPoint x board) coords


-- Calculates proints for word using
-- start index and end index of board
calculatePoints :: (Int, Int) -> (Int, Int) -> Board -> Int
calculatePoints from to board = calculatePointsFromList (getRowPairs from to) board


commonValidate :: (Int, Int) -> Bool
commonValidate x = fst x >= 0 && fst x <= 14 && snd x >= 0 && snd x <= 14


-- Validates if list of numbers is chained: (1,2,3,4,5), not (1, 3, 6,)
validateChainSorted :: [Int] -> Bool
validateChainSorted [] = True
validateChainSorted [a] = True
validateChainSorted (a:an:as) = a + 1 == an && (validateChain $ [an] ++ as)


validateChain :: [Int] -> Bool
validateChain = validateChainSorted . sort


validateVertical :: [(Int, Int)] -> Bool
validateVertical [] = True
validateVertical [a] = True
validateVertical lst = validateChain (map snd lst) && (all (\x -> x == fst a) $ map fst lst) && all commonValidate lst
        where a = lst !! 0


validateHorizontal :: [(Int, Int)] -> Bool
validateHorizontal [] = True
validateHorizontal [a] = True
validateHorizontal lst = validateChain (map fst lst) && (all (\x -> x == snd a) $ map snd lst) && all commonValidate lst
        where a = lst !! 0


validateIndexes :: [(Int, Int)] -> Bool
validateIndexes idxs = validateHorizontal idxs || validateVertical idxs
