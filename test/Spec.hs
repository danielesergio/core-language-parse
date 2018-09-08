module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Lib
import Model
import CoreParser

main :: IO ()
main = do
  defaultMain (testGroup "Our Library Tests" [sayYoTest, test1, test2, test3])

sayYoTest :: TestTree
sayYoTest = testCase "Testing sayYo"
  (assertEqual "Should say Yo to Friend!" "Yo Friend!" ("Yo Friend!"))

doublingMakesNumbersBigger =
  testCase "Double of 4 is 8" $ assertEqual [] 8 (4 * 4)

test1 :: TestTree
test1 = testCase "test alphanumWithUnderScore success with 123" (assertEqual [] (parse alphanumWithUnderScore "123") [('1',"23")] )

test2 :: TestTree
test2 = testCase "test alphanumWithUnderScore success with _a23" (assertEqual [] (parse alphanumWithUnderScore "_a23") [('_',"a23")] )

test3 :: TestTree
test3 = testCase "test alphanumWithUnderScore fail with  +a23" (assertEqual [] (parse alphanumWithUnderScore "+a23") [] )


