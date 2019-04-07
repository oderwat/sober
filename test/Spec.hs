{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Data.Text (Text(..))

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Brewer.Internal

main :: IO ()
main = hspec $ do

     describe "Brewer.Internal.separateNameVersion" $ do
       it "splits a string at the first space" $ do
         splitAtFirstSpace "Test 123" `shouldBe` Just ("Test", "123")

       it "returns Nothing if there is no space" $ do
         splitAtFirstSpace "Test-123" `shouldBe` Nothing

       it "returns Nothing if there is only spaces" $ do
         splitAtFirstSpace "     " `shouldBe` Nothing

       it "returns Nothing if the string is empty" $ do
         splitAtFirstSpace "" `shouldBe` Nothing

       it "it ignores trailing spaces" $ do
         splitAtFirstSpace "  Test 123" `shouldBe` Just ("Test", "123")

       it "it ignores multiple spaces in the separation" $ do
         splitAtFirstSpace "Test   123" `shouldBe` Just ("Test", "123")

       it "it ignores trailing spaces" $ do
         splitAtFirstSpace "Test 123    " `shouldBe` Just ("Test", "123")
