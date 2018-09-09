module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Lib
import Model
import CoreParser

main :: IO ()
main = do
  defaultMain (testGroup "Our Library Tests"[
    sayYoTest,
    testAlphanumWithUnderScore1, testAlphanumWithUnderScore2, testAlphanumWithUnderScore3,
    testParseAExpr1, testParseAExpr2, testParseAExpr3, testParseAExpr4, testParseAExpr5, testParseAExpr6, testParseAExpr7, testParseAExpr8, testParseAExpr9])

sayYoTest :: TestTree
sayYoTest = testCase "Testing sayYo"
  (assertEqual "Should say Yo to Friend!" "Yo Friend!" ("Yo Friend!"))

doublingMakesNumbersBigger =
  testCase "Double of 4 is 8" $ assertEqual [] 8 (4 * 4)

testAlphanumWithUnderScore1 :: TestTree
testAlphanumWithUnderScore1 = testCase "test alphanumWithUnderScore success with 123" (assertEqual [] [('1',"23")] (parse alphanumWithUnderScore "123"))

testAlphanumWithUnderScore2 :: TestTree
testAlphanumWithUnderScore2 = testCase "test alphanumWithUnderScore success with _a23" (assertEqual [] [('_',"a23")] (parse alphanumWithUnderScore "_a23") )

testAlphanumWithUnderScore3 :: TestTree
testAlphanumWithUnderScore3 = testCase "test alphanumWithUnderScore fail with  +a23" (assertEqual [] [] (parse alphanumWithUnderScore "+a23")  )

testParseAExpr1 :: TestTree
testParseAExpr1 = testCase "test parseAExpr success with  \"a_23_ aa\"" (assertEqual [] [(Evar "a_23_","aa")] (parse parseAExpr "a_23_ aa"))

testParseAExpr2 :: TestTree
testParseAExpr2 = testCase "test parseAExpr success with  \" a23aa \"" (assertEqual [] [(Evar "a23aa","")] (parse parseAExpr " a23aa ") )

testParseAExpr3 :: TestTree
testParseAExpr3 = testCase "test parseAExpr success with  1a23" (assertEqual [] [(ENum 1,"a23")] (parse parseAExpr "1a23")  )

testParseAExpr4 :: TestTree
testParseAExpr4 = testCase "test parseAExpr fail with  _a23" (assertEqual [] [] (parse parseAExpr "_a23")  )

testParseAExpr5 :: TestTree
testParseAExpr5 = testCase "test parseAExpr success with  \"121 _asdf \"" (assertEqual [] [(ENum 121,"_asdf ")] (parse parseAExpr "121 _asdf ") )

testParseAExpr6 :: TestTree
testParseAExpr6 = testCase "test parseAExpr success with  \"Pack{121,0}_asdf \"" (assertEqual [] [(EConstr 121 0,"_asdf ")] (parse parseAExpr "Pack{121,0}_asdf ") )

testParseAExpr7 :: TestTree
testParseAExpr7 = testCase "test parseAExpr success with  \"(Pack{121,0})_asdf \"" (assertEqual [] [(EConstr 121 0,"_asdf ")] (parse parseAExpr "(Pack{121,0})_asdf ") )

testParseAExpr8 :: TestTree
testParseAExpr8 = testCase "test parseAExpr success with  \"(121) _asdf \"" (assertEqual [] [(ENum 121,"_asdf ")] (parse parseAExpr "(121) _asdf ") )

testParseAExpr9 :: TestTree
testParseAExpr9 = testCase "test parseAExpr success with  \"(var)121 _asdf \"" (assertEqual [] [(Evar "var","121 _asdf ")] (parse parseAExpr "(var)121 _asdf ") )



