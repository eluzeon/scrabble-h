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
  "checkState" :> Capture "gameNumber" Integer :> Get '[JSON] ResponseForWhileTrue

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
	exampleAction

initGame :: Handler ResponseForInitGame
initGame = return $ ResponseForInitGame 4 2 [['a'], ['b'], ['c'], ['d'], ['e'], ['f'], ['g']]

exampleAction :: Integer -> Handler ResponseForWhileTrue
exampleAction 4 = return $ ResponseForWhileTrue True 2 10 0 'w'
exampleAction _ = return $ ResponseForWhileTrue False 1 10 0 'w'

exampleResponse :: String
exampleResponse = "Oh shit, it works!"

data ResponseForWhileTrue
  = ResponseForWhileTrue {
    isStateChanged :: Bool,
    playerTurnNumber :: Integer,
    positionX :: Integer,
    positionY :: Integer,
    letter :: Char
  }
  deriving (Eq, Show, Generic)

instance ToJSON ResponseForWhileTrue
instance FromJSON ResponseForWhileTrue

data ResponseForInitGame
  = ResponseForInitGame {
    gameNumber :: Integer,
    playerNumber :: Integer,
    letters :: [String]
  }
  deriving (Eq, Show, Generic)

instance ToJSON ResponseForInitGame
instance FromJSON ResponseForInitGame
