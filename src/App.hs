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
import Control.Monad.IO.Class
import System.Directory (doesFileExist, doesDirectoryExist)
import WebApiTypes
import Extensions

-- * api

type ExampleApi = "initGame" :> Get '[JSON] ResponseForInitGame :<|>
  "checkState" :> Capture "gameNumber" String :> Get '[JSON] ResponseForWhileTrue :<|>
  "sendChanges" :> ReqBody '[JSON] ChangesForSendChanges :> Post '[JSON] [String] :<|>
  "changeLetters" :> ReqBody '[JSON] SkipTurnBody :> Post '[JSON] [String] :<|>
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
initGame = do
    liftIO $ doesFileExist ("1" ++ ".json") >>= \case
        True -> putStrLn "Yes" 
        False -> encodeFile ("1" ++ ".json") $ ObjectForSingleGame (ResponseForWhileTrue False 1 0 [] [Changes 0 0 ' ']) emptyBoard defaultLettersPool
    
    item <- liftIO $ decodeFileStrict ("1" ++ ".json") :: Handler (Maybe ObjectForSingleGame)
    
    case item of
        Nothing -> throwError err500
        Just (ObjectForSingleGame (ResponseForWhileTrue isGameStarted playerTurnNumber numberOfPlayers playersPoints changes) board letters) -> do
            liftIO $ encodeFile ("1" ++ ".json") $ ObjectForSingleGame (ResponseForWhileTrue isGameStarted playerTurnNumber (1 + numberOfPlayers) (playersPoints ++ [0]) changes) board letters
            return $ ResponseForInitGame (PlayerAndGameInfo "1" $ 1 + numberOfPlayers) [['a'], ['b'], ['c'], ['d'], ['e'], ['f'], ['g']]



initGameLoop :: Int -> Handler ResponseForInitGame
initGameLoop _ = return $ ResponseForInitGame (PlayerAndGameInfo "1" 1) [['a'], ['b'], ['c'], ['d'], ['e'], ['f'], ['g']]



checkState :: String -> Handler ResponseForWhileTrue
checkState gameNumber = do
    item <- liftIO $ decodeFileStrict (gameNumber ++ ".json") :: Handler (Maybe ObjectForSingleGame)

    case item of
        Nothing -> throwError err422
        Just (ObjectForSingleGame responseForWhileTrue _ _) -> return $ responseForWhileTrue



changeState :: ChangesForSendChanges -> Handler [String]
changeState (ChangesForSendChanges allChanges (PlayerAndGameInfo gameNumber playerNumber)) = do
    item <- liftIO $ decodeFileStrict (gameNumber ++ ".json") :: Handler (Maybe ObjectForSingleGame)

    case item of
        Nothing -> throwError err422
        Just (ObjectForSingleGame (ResponseForWhileTrue isGameStarted playerTurnNumber numberOfPlayers playersPoints changes) board letters) -> do
            let nextPlayer = getNextPlayer playerTurnNumber numberOfPlayers
            liftIO $ encodeFile (gameNumber ++ ".json") $ ObjectForSingleGame (ResponseForWhileTrue isGameStarted nextPlayer numberOfPlayers playersPoints allChanges) board letters
            result <- liftIO $ returnNLettersFromGetLetters 0 (length letters) letters (length allChanges)
            return $ result



changeLetters :: SkipTurnBody -> Handler [String]
changeLetters (SkipTurnBody gameNumber n) = do
    item <- liftIO $ decodeFileStrict (gameNumber ++ ".json") :: Handler (Maybe ObjectForSingleGame)
    
    case item of
        Nothing -> throwError err500
        Just (ObjectForSingleGame (ResponseForWhileTrue isGameStarted playerTurnNumber numberOfPlayers playersPoints changes) board letters) -> do
            let nextPlayer = getNextPlayer playerTurnNumber numberOfPlayers
            liftIO $ encodeFile (gameNumber ++ ".json") $ ObjectForSingleGame (ResponseForWhileTrue isGameStarted nextPlayer numberOfPlayers playersPoints changes) board letters
            result <- liftIO $ returnNLettersFromGetLetters 0 (length letters) letters n
            return result



startGame :: String -> Handler ()
startGame gameNumber = do
    item <- liftIO $ decodeFileStrict (gameNumber ++ ".json") :: Handler (Maybe ObjectForSingleGame)

    case item of
        Nothing -> throwError err422
        Just (ObjectForSingleGame (ResponseForWhileTrue isGameStarted playerTurnNumber numberOfPlayers playersPoints changes) board letters) -> do
            liftIO $ encodeFile (gameNumber ++ ".json") $ ObjectForSingleGame (ResponseForWhileTrue True playerTurnNumber numberOfPlayers playersPoints changes) board letters
            return ()



