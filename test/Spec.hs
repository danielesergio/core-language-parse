module Main where

import Test.Tasty

import Test.Tasty.HUnit

main :: IO ()
main = do
  defaultMain (testGroup "Our Library Tests" [sayYoTest])

sayYoTest :: TestTree
sayYoTest = testCase "Testing sayYo"
  (assertEqual "Should say Yo to Friend!" "Yo Friend!" ("Yo Friend!"))