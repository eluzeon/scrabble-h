{-# LANGUAGE DeriveGeneric #-}

module WebApiTypes where

import Data.Aeson
import Game.Types
import GHC.Generics


data ObjectForSingleGame
  = ObjectForSingleGame {
    playersAndChangesInfo :: ResponseForWhileTrue,
    board :: [[Point]],
    lettersPool :: [String]
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
    letter :: String
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
    gameNumber :: String,
    playerNumber :: Int
  }
  deriving (Eq, Show, Generic)

instance ToJSON PlayerAndGameInfo
instance FromJSON PlayerAndGameInfo

data SkipTurnBody
  = SkipTurnBody {
    gameNumberForSkipTurn :: String,
    countToChange :: Int
  }
  deriving (Eq, Show, Generic)

instance ToJSON SkipTurnBody
instance FromJSON SkipTurnBody

data LettersArrayToReturn
  = LettersArrayToReturn {
    lettersResult :: [String]
  }
  deriving (Eq, Show, Generic)

instance ToJSON LettersArrayToReturn
instance FromJSON LettersArrayToReturn
