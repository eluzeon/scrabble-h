{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module App where

import           Data.Aeson
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO

-- * api

type ExampleApi = "initGame" :> Get '[JSON] ResponseForInitGame :<|>
  "checkState" :> Capture "gameNumber" Int :> Get '[JSON] ResponseForWhileTrue :<|>
  "sendChanges" :> ReqBody '[JSON] ChangesForSendChanges :> Post '[JSON] [String] :<|>
  "changeLetters" :> Capture "countToChange" Int :> Post '[JSON] [String]

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
	changeLetters

initGame :: Handler ResponseForInitGame
initGame = return $ ResponseForInitGame (PlayerAndGameInfo 4 1) [['a'], ['b'], ['c'], ['d'], ['e'], ['f'], ['g']]

checkState :: Int -> Handler ResponseForWhileTrue
checkState _ = return $ ResponseForWhileTrue False 1 2 [13, 10] $ Changes 10 0 'w'

changeState :: ChangesForSendChanges -> Handler [String]
changeState changes = return $ take (length $ allChanges changes) [['x'], ['m'], ['v'], ['n'], ['s'], ['q'], ['o']]

changeLetters :: Int -> Handler [String]
changeLetters n = return $ take n [['x'], ['m'], ['v'], ['n'], ['s'], ['q'], ['o']]

data ResponseForWhileTrue
  = ResponseForWhileTrue {
    isStateChanged :: Bool,
    playerTurnNumber :: Int,
    numberOfPlayers :: Int,
    points :: [Int],
    changes :: Changes
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
