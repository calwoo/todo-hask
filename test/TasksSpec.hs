module TasksSpec where

import Tasks

import Test.Hspec (Spec, describe, context, it, shouldBe)

spec :: Spec
spec = 
    describe "Task Data Types" $ do
        describe "Date" $ do
            it "Shows YYYY-MM-DD with single digit month and day" $ do
                show (Date 2017 2 1) `shouldBe` "2017-02-01"
        
            it "Shows YYYY-MM-DD with double digit month and day" $ do
                show (Date 2017 12 11) `shouldBe` "2017-12-11"


