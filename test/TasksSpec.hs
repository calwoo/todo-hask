module TasksSpec where

import Tasks

import Test.Hspec (Spec, describe, context, it, shouldBe)

spec :: Spec
spec = 
    describe "Task Data Types" $ do
        describe "Date" $ do
            it "Shows YYYY-MM-DD with single digit month and day" $ do
                show (Date 2 1 2017) `shouldBe` "02-01-2017"
        
            it "Shows YYYY-MM-DD with double digit month and day" $ do
                show (Date 12 11 2017) `shouldBe` "12-11-2017"


