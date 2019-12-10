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
        False -> Data.Aeson.encodeFile ("1" ++ ".json") $ ObjectForSingleGame False (ResponseForWhileTrue 1 2 [0] $ [Changes 0 0 ' ']) emptyBoard
    
    item <- liftIO $ decodeFileStrict ("1" ++ ".json") :: Handler (Maybe ObjectForSingleGame)
    
    case item of
        Nothing -> return $ ResponseForInitGame (PlayerAndGameInfo 1 1) [['a'], ['b'], ['c'], ['d'], ['e'], ['f'], ['g']]
        Just obj -> return $ ResponseForInitGame (PlayerAndGameInfo 1 $ (+) 1 $ numberOfPlayers $ playersAndChangesInfo obj) [['a'], ['b'], ['c'], ['d'], ['e'], ['f'], ['g']]

initGameLoop :: String -> Handler ResponseForInitGame
initGameLoop _ = return $ ResponseForInitGame (PlayerAndGameInfo 1 1) [['a'], ['b'], ['c'], ['d'], ['e'], ['f'], ['g']]

checkState :: String -> Handler ResponseForWhileTrue
checkState gameNumber = return $ ResponseForWhileTrue 1 2 [13, 10] $ [Changes 10 0 'w']

changeState :: ChangesForSendChanges -> Handler [String]
changeState changes = return $ take (length $ allChanges changes) [['x'], ['m'], ['v'], ['n'], ['s'], ['q'], ['o']]

changeLetters :: Int -> Handler [String]
changeLetters n = return $ take n [['x'], ['m'], ['v'], ['n'], ['s'], ['q'], ['o']]

startGame :: String -> Handler ()
startGame gameNumber =
    return ()

data ObjectForSingleGame
  = ObjectForSingleGame {
    isGameStarted :: Bool,
    playersAndChangesInfo :: ResponseForWhileTrue,
    board :: [[Point]]
  }
  deriving (Eq, Show, Generic)

instance ToJSON ObjectForSingleGame
instance FromJSON ObjectForSingleGame

data ResponseForWhileTrue
  = ResponseForWhileTrue {
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
