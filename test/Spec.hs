{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           Data.Char
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO
import           Control.Monad
import           Control.Monad.IO.Class

import           Servant.Client
import           Servant.Server
import           Servant.QuickCheck

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary
import           App
import           WebApiTypes
import           Game
import           Test.Tasty

main :: IO ()
main = do 
  --hspec test1
  hspec test3
  hspec test2
  defaultMain gameTests


args :: Args
args = defaultArgs { maxSuccess = 1, chatty = True }


instance Arbitrary ChangesForSendChanges where 
  arbitrary = do
    Positive x <- arbitrary
    Positive y <- arbitrary
    let ch = Changes x y "a"
    return $ ChangesForSendChanges [ch] $ PlayerAndGameInfo "1" 1  

instance Arbitrary SkipTurnBody where
  arbitrary = do
    Positive x <- arbitrary
    return $ SkipTurnBody "1" x


test1 :: Spec
test1 = describe "exampleApi" $
  it "not500" $ 
    withServantServer exampleApi (return server) $ \burl ->
      serverSatisfies exampleApi burl args
        ( not500 <%> mempty)
test2 :: Spec
test2 = describe "exampleApi" $
  it "onlyJSON" $ 
    withServantServer exampleApi (return server) $ \burl ->
      serverSatisfies exampleApi burl args
        ( onlyJsonObjects <%> mempty)
test3 :: Spec
test3 = describe "exampleApi" $
  it "timeout" $ 
    withServantServer exampleApi (return server) $ \burl ->
      serverSatisfies exampleApi burl args
        ( notLongerThan 15000000000 <%> mempty) --nanoseconds
  