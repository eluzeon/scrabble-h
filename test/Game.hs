module Game where

import Test.Tasty
import Test.Tasty.HUnit
import Game.Logic
import Game.Types

-- Chain tests
testOfValidationChainCorrect = testCase "Correct chain testing" $ assertBool [] $ validateChain [1,2,3,4,5]
testOfValidationChainUnsorted = testCase "Correct chain unsorted tessting" $ assertBool [] $ validateChain [2, 4, 3, 1, 5]  
testOfValidationChainIsIncorrent = testCase "Incorrect chain testing" $ assertBool [] $ not $ validateChain [1, 3, 4, 5]


-- points validation
testOfPointsVerticalValid = testCase "Vertical valid points" $ assertBool [] $ validateVertical [(1,2), (1, 3), (1,5), (1,4)]
testOfPointsOutOfRange = testCase "Vertical validation out of range" $ assertBool [] $ not $ validateVertical [(15, 1), (15, 2), (15,3)]
testOfPointsNotVertical = testCase "Vertical validation not vertical" $ assertBool [] $ not $ validateVertical [(3, 1), (2, 1), (3, 2)]


-- points horizontall
testOfPointsHorizontalValid = testCase "Horizontal valid points" $ assertBool [] $ validateHorizontal [(6, 7), (7, 7), (8, 7), (9, 7)]
testOfPointsHorizontalOutOfRange = testCase "Horizontal out of range points" $ assertBool [] $ not $ validateHorizontal [(9, 1), (10, 1), (11, 1), (12, 1), (13, 1), (14, 1), (15,1)]
testOfPointsHorizontalInvalid = testCase "Horizontal invalid" $ assertBool [] $ not $ validateHorizontal [(4, 1), (4, 2), (4, 3), (4,4)]


-- global validateIndexes
testOfValidateIndexesVert = testCase "ValidateIndexes vertical is valid" $ assertBool [] $ validateIndexes [(10, 1), (10, 2), (10, 3)]
testOfValidateIndexesHor = testCase "ValidateIndexes horizontal is valid" $ assertBool [] $ validateIndexes [(1, 9), (2, 9), (3, 9), (4,9)]


testOfRowsPair = testCase "Valid Rows pairs" $ assertEqual [] [(10,5),(10,6),(10,7),(10,8),(10,9)] $ getRowPairs (10, 5) (10, 9)
testOfRowsPairNotCorrect = testCase "Invalid Rows pairs" $ assertEqual [] [] $ getRowPairs (10, 5) (10, 1)

validationChainTests = testGroup
     "chainValidation tests" 
        [testOfValidationChainCorrect, testOfValidationChainIsIncorrent, testOfValidationChainUnsorted]


verticalValidationTests = testGroup
    "verticalValidation tests"
        [testOfPointsNotVertical, testOfPointsOutOfRange, testOfPointsNotVertical]

horizontalValidationTest = testGroup
    "verticalHorizontal test"
        [testOfPointsHorizontalValid, testOfPointsHorizontalInvalid, testOfPointsHorizontalOutOfRange]

utilsTests = testGroup
    "utils functions"
        [testOfRowsPair, testOfRowsPairNotCorrect, testOfRowsPair, testOfRowsPairNotCorrect, testOfValidateIndexesHor, testOfValidateIndexesVert]


-- Test of types
testOfDoubleRow = testCase "Valid doubleRow" $ assertEqual [] [1,2,3,4,3,2,1] $ doubleRow [1,2,3,4]
testOfDoubleRowIncorrect = testCase "Valid single doubleRow" $ assertEqual [] [1] $ doubleRow [1]

testOfGetSymbPointsA = testCase "Valid Symbol points for A" $ assertEqual [] 1 $ getSymbPoints A
testOfGetSymbPointsQ = testCase "Valid Symbol points for Q"  $ assertEqual [] 10 $ getSymbPoints Q
testOfGetSymbPointsK = testCase "Valid Symbol points for K" $ assertEqual [] 5 $ getSymbPoints K


testOfParser = testCase "Valid parsing of Z" $ assertEqual [] Z $ fromString "Z"
testOfIndexParser = testCase "Valid parsing from index" $ assertEqual [] X $ fromIndex 23



typesTests = testGroup
    "types functions tests"
        [testOfDoubleRow, testOfDoubleRowIncorrect, testOfGetSymbPointsA,
        testOfGetSymbPointsK, testOfGetSymbPointsQ, testOfParser, testOfIndexParser]


gameTests = testGroup 
    "total game tests"
        [validationChainTests, verticalValidationTests, horizontalValidationTest, utilsTests, typesTests]
