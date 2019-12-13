{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module App where

import Data.Aeson
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO
import Game.Types
import Game.Logic
import Control.Lens
import Control.Monad.IO.Class
import System.Directory (doesFileExist, doesDirectoryExist)
import WebApiTypes
import Extensions

-- * api

type ExampleApi = "initGame" :> Get '[JSON] ResponseForInitGame :<|>
  "checkState" :> Capture "gameNumber" String :> Get '[JSON] ResponseForWhileTrue :<|>
  "sendChanges" :> ReqBody '[JSON] ChangesForSendChanges :> Post '[JSON] LettersArrayToReturn :<|>
  "changeLetters" :> ReqBody '[JSON] SkipTurnBody :> Post '[JSON] LettersArrayToReturn :<|>
  "startGame" :> Capture "gameNumber" String :> Post '[JSON] ()

exampleApi :: Proxy ExampleApi
exampleApi = Proxy

-- * app

run :: IO ()
run = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve exampleApi server

server :: Server ExampleApi
server = 
    initGame :<|>
    checkState :<|>
    changeState :<|>
    changeLetters :<|>
    startGame

initGame :: Handler ResponseForInitGame
initGame = initGameLoop 1




initGameLoop :: Int -> Handler ResponseForInitGame
initGameLoop gameNumber = do
    let strNumber = (show gameNumber)
    liftIO $ doesFileExist (strNumber ++ ".json") >>= \case
        True -> return ()
        False -> encodeFile (strNumber ++ ".json") $ ObjectForSingleGame (ResponseForWhileTrue False 1 0 [] []) emptyBoard defaultLettersPool
    
    item <- liftIO $ decodeFileStrict (strNumber ++ ".json") :: Handler (Maybe ObjectForSingleGame)
    
    case item of
        Nothing -> initGameLoop (gameNumber + 1)
        Just gameObj -> checkNextGameOrJoinThis gameNumber gameObj

checkNextGameOrJoinThis :: Int -> ObjectForSingleGame -> Handler ResponseForInitGame
checkNextGameOrJoinThis gameNumber (ObjectForSingleGame (ResponseForWhileTrue isGameStarted playerTurnNumber numberOfPlayers playersPoints changes) board letters)
    | isGameStarted || (numberOfPlayers == 4) = initGameLoop (gameNumber + 1)
    | otherwise = do
        let strNumber = (show gameNumber)
        (TakeNLettersDto remaining lettersResult) <- liftIO $ returnNLettersFromGetLetters 0 (length letters) letters 7
        liftIO $ encodeFile (strNumber ++ ".json") $ ObjectForSingleGame (ResponseForWhileTrue isGameStarted playerTurnNumber (1 + numberOfPlayers) (playersPoints ++ [0]) changes) board remaining
        return $ ResponseForInitGame (PlayerAndGameInfo strNumber $ 1 + numberOfPlayers) lettersResult

checkState :: String -> Handler ResponseForWhileTrue
checkState gameNumber = do
    item <- liftIO $ decodeFileStrict (gameNumber ++ ".json") :: Handler (Maybe ObjectForSingleGame)

    case item of
        Nothing -> throwError err422
        Just (ObjectForSingleGame responseForWhileTrue _ _) -> return $ responseForWhileTrue



changeState :: ChangesForSendChanges -> Handler LettersArrayToReturn
changeState (ChangesForSendChanges allChanges (PlayerAndGameInfo gameNumber playerNumber)) = do
    item <- liftIO $ decodeFileStrict (gameNumber ++ ".json") :: Handler (Maybe ObjectForSingleGame)

    case item of
        Nothing -> throwError err422
        Just (ObjectForSingleGame (ResponseForWhileTrue isGameStarted playerTurnNumber numberOfPlayers playersPoints changes) board letters) -> do
            let nextPlayer = getNextPlayer playerTurnNumber numberOfPlayers
            let newBoard = applyChanges allChanges board
            let newPoints = playersPoints & element (playerTurnNumber - 1) +~ calculatePointsFromList (fromChanges allChanges) newBoard
            (TakeNLettersDto remaining result) <- liftIO $ returnNLettersFromGetLetters 0 (length letters) letters (length allChanges)
            liftIO $ encodeFile (gameNumber ++ ".json") $ ObjectForSingleGame (ResponseForWhileTrue isGameStarted nextPlayer numberOfPlayers newPoints allChanges) newBoard remaining
            return $ LettersArrayToReturn result



changeLetters :: SkipTurnBody -> Handler LettersArrayToReturn
changeLetters (SkipTurnBody gameNumber n) = do
    item <- liftIO $ decodeFileStrict (gameNumber ++ ".json") :: Handler (Maybe ObjectForSingleGame)
    
    case item of
        Nothing -> throwError err404
        Just (ObjectForSingleGame (ResponseForWhileTrue isGameStarted playerTurnNumber numberOfPlayers playersPoints changes) board letters) -> do
            let nextPlayer = getNextPlayer playerTurnNumber numberOfPlayers
            (TakeNLettersDto remaining result) <- liftIO $ returnNLettersFromGetLetters 0 (length letters) letters n
            liftIO $ encodeFile (gameNumber ++ ".json") $ ObjectForSingleGame (ResponseForWhileTrue isGameStarted nextPlayer numberOfPlayers playersPoints changes) board remaining
            return $ LettersArrayToReturn result



startGame :: String -> Handler ()
startGame gameNumber = do
    item <- liftIO $ decodeFileStrict (gameNumber ++ ".json") :: Handler (Maybe ObjectForSingleGame)

    case item of
        Nothing -> throwError err422
        Just (ObjectForSingleGame (ResponseForWhileTrue isGameStarted playerTurnNumber numberOfPlayers playersPoints changes) board letters) -> do
            liftIO $ encodeFile (gameNumber ++ ".json") $ ObjectForSingleGame (ResponseForWhileTrue True playerTurnNumber numberOfPlayers playersPoints changes) board letters
            return ()



