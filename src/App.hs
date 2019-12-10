{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module App where

import Data.Aeson
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO
import Game.Types
import Control.Monad.IO.Class (liftIO)
import System.Directory (doesFileExist, doesDirectoryExist)

-- * api

type ExampleApi = "initGame" :> Get '[JSON] ResponseForInitGame :<|>
  "checkState" :> Capture "gameNumber" String :> Get '[JSON] ResponseForWhileTrue :<|>
  "sendChanges" :> ReqBody '[JSON] ChangesForSendChanges :> Post '[JSON] [String] :<|>
  "changeLetters" :> Capture "countToChange" Int :> Post '[JSON] [String] :<|>
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
        False -> encodeFile ("1" ++ ".json") $ ObjectForSingleGame (ResponseForWhileTrue False 1 0 [] [Changes 0 0 ' ']) emptyBoard
    
    item <- liftIO $ decodeFileStrict ("1" ++ ".json") :: Handler (Maybe ObjectForSingleGame)
    
    case item of
        Nothing -> throwError err500
        Just (ObjectForSingleGame (ResponseForWhileTrue isGameStarted playerTurnNumber numberOfPlayers playersPoints changes) board) -> do
            liftIO $ encodeFile ("1" ++ ".json") $ ObjectForSingleGame (ResponseForWhileTrue isGameStarted playerTurnNumber (1 + numberOfPlayers) (playersPoints ++ [0]) changes) emptyBoard
            return $ ResponseForInitGame (PlayerAndGameInfo 1 $ 1 + numberOfPlayers) [['a'], ['b'], ['c'], ['d'], ['e'], ['f'], ['g']]

initGameLoop :: Int -> Handler ResponseForInitGame
initGameLoop _ = return $ ResponseForInitGame (PlayerAndGameInfo 1 1) [['a'], ['b'], ['c'], ['d'], ['e'], ['f'], ['g']]



checkState :: String -> Handler ResponseForWhileTrue
checkState gameNumber = do
    item <- liftIO $ decodeFileStrict (gameNumber ++ ".json") :: Handler (Maybe ObjectForSingleGame)

    case item of
        Nothing -> throwError err422
        Just (ObjectForSingleGame responseForWhileTrue board) -> return $ responseForWhileTrue



changeState :: ChangesForSendChanges -> Handler [String]
changeState (allChanges (gameNumber playerNumber)) = do
    item <- liftIO $ decodeFileStrict (gameNumber ++ ".json") :: Handler (Maybe ObjectForSingleGame)

    case item of
        Nothing -> throwError err422
        Just (ObjectForSingleGame (ResponseForWhileTrue isGameStarted playerTurnNumber numberOfPlayers playersPoints changes) board) -> do
            (let nextPlayer = case (playerTurnNumber == numberOfPlayers) of
              True -> 1
              False -> (1 + playerTurnNumber)
            liftIO $ encodeFile (gameNumber ++ ".json") $ ObjectForSingleGame (ResponseForWhileTrue isGameStarted nextPlayer numberOfPlayers playersPoints allChanges) board)
            return $ take (length allChanges) [['x'], ['m'], ['v'], ['n'], ['s'], ['q'], ['o']]



changeLetters :: Int -> Handler [String]
changeLetters n = return $ take n [['x'], ['m'], ['v'], ['n'], ['s'], ['q'], ['o']]



startGame :: String -> Handler ()
startGame gameNumber = do
    item <- liftIO $ decodeFileStrict (gameNumber ++ ".json") :: Handler (Maybe ObjectForSingleGame)

    case item of
        Nothing -> throwError err422
        Just (ObjectForSingleGame (ResponseForWhileTrue isGameStarted playerTurnNumber numberOfPlayers playersPoints changes) board) -> do
            liftIO $ encodeFile (gameNumber ++ ".json") $ ObjectForSingleGame (ResponseForWhileTrue True playerTurnNumber numberOfPlayers playersPoints changes) board
            return ()



data ObjectForSingleGame
  = ObjectForSingleGame {
    playersAndChangesInfo :: ResponseForWhileTrue,
    board :: [[Point]]
  }
  deriving (Eq, Show, Generic)

instance ToJSON ObjectForSingleGame
instance FromJSON ObjectForSingleGame

data ResponseForWhileTrue
  = ResponseForWhileTrue {
    isGameStarted :: Bool,
    playerTurnNumber :: Int,
    numberOfPlayers :: Int,
    playersPoints :: [Int],
    changes :: [Changes]
  }
  deriving (Eq, Show, Generic)

instance ToJSON ResponseForWhileTrue
instance FromJSON ResponseForWhileTrue

data ResponseForInitGame
  = ResponseForInitGame {
    playerAndGameInfo :: PlayerAndGameInfo,
    letters :: [String]
  }
  deriving (Eq, Show, Generic)

instance ToJSON ResponseForInitGame
instance FromJSON ResponseForInitGame

data Changes
  = Changes {
    positionX :: Int,
    positionY :: Int,
    letter :: Char
  }
  deriving (Eq, Show, Generic)

instance ToJSON Changes
instance FromJSON Changes

data ChangesForSendChanges
  = ChangesForSendChanges {
    allChanges :: [Changes],
    info :: PlayerAndGameInfo
  }
  deriving (Eq, Show, Generic)

instance ToJSON ChangesForSendChanges
instance FromJSON ChangesForSendChanges

data PlayerAndGameInfo
  = PlayerAndGameInfo {
    gameNumber :: Int,
    playerNumber :: Int
  }
  deriving (Eq, Show, Generic)

instance ToJSON PlayerAndGameInfo
instance FromJSON PlayerAndGameInfo
