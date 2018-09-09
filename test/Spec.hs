module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Lib
import Model
import CoreParser
import Text.Printf

main :: IO ()
main = do
  defaultMain (testGroup "Our Library Tests"[
    testAlphanumWithUnderScore1, testAlphanumWithUnderScore2, testAlphanumWithUnderScore3,
    testParseAExpr1, testParseAExpr2, testParseAExpr3, testParseAExpr4, testParseAExpr5, testParseAExpr6, testParseAExpr7,
    testParseAExpr8, testParseAExpr9, testParseAExpr10, testParseAExpr11,
    testParseDef1, testParseDef2, testParseDef3, testParseDef4,
    testParseAlt1, testParseAlt2, testParseAlt3, testParseAlt4, testParseAlt5,
    testParseList1, testParseList2, testParseList3, testParseList4
    ])


assertEqualTestTemplate :: (Eq b, Show b) => String -> String -> (String -> b) -> b -> TestTree
assertEqualTestTemplate desc inp f result = testCase (printf "test %s with \"%s\" (expeted: %s)" desc inp (show result)) (assertEqual [] result (f inp))

testAlphanumWithUnderScore1 :: TestTree
testAlphanumWithUnderScore1 = assertEqualTestTemplate "alphanumWithUnderScore successes" "123" (parse alphanumWithUnderScore) [('1',"23")]

testAlphanumWithUnderScore2 :: TestTree
testAlphanumWithUnderScore2 = assertEqualTestTemplate "alphanumWithUnderScore successes" "_a23" (parse alphanumWithUnderScore) [('_',"a23")]

testAlphanumWithUnderScore3 :: TestTree
testAlphanumWithUnderScore3 = assertEqualTestTemplate "alphanumWithUnderScore fails" "+a23" (parse alphanumWithUnderScore) []

testParseAExpr1 :: TestTree
testParseAExpr1 = assertEqualTestTemplate "parseAExpr successes with" "a_23_ aa" (parse parseAExpr) [(EVar "a_23_","aa")]

testParseAExpr2 :: TestTree
testParseAExpr2 = assertEqualTestTemplate "parseAExpr successes with" " a23aa " (parse parseAExpr) [(EVar "a23aa","")]

testParseAExpr3 :: TestTree
testParseAExpr3 = assertEqualTestTemplate "parseAExpr successes with" " 1a23 " (parse parseAExpr) [(ENum 1,"a23 ")]

testParseAExpr4 :: TestTree
testParseAExpr4 = assertEqualTestTemplate "parseAExpr fails with" "_a23" (parse parseAExpr) []

testParseAExpr5 :: TestTree
testParseAExpr5 = assertEqualTestTemplate "parseAExpr successes with" "121 _asdf " (parse parseAExpr)  [(ENum 121,"_asdf ")]

testParseAExpr6 :: TestTree
testParseAExpr6 = assertEqualTestTemplate "parseAExpr successes with" "Pack{121,0}_asdf " (parse parseAExpr)  [(EConstr 121 0,"_asdf ")]

testParseAExpr7 :: TestTree
testParseAExpr7 = assertEqualTestTemplate "parseAExpr successes with" "(Pack{121,0})_asdf " (parse parseAExpr)  [(EConstr 121 0,"_asdf ")]

testParseAExpr8 :: TestTree
testParseAExpr8 = assertEqualTestTemplate "parseAExpr successes with" "(121) _asdf " (parse parseAExpr)  [(ENum 121,"_asdf ")]

testParseAExpr9 :: TestTree
testParseAExpr9 = assertEqualTestTemplate "parseAExpr successes with" "(var)121 _asdf " (parse parseAExpr)  [(EVar "var","121 _asdf ")]

testParseAExpr10 :: TestTree
testParseAExpr10 = assertEqualTestTemplate "parseAExpr fails with" "Pack 121 _asdf " (parse parseAExpr) []

testParseAExpr11 :: TestTree
testParseAExpr11 = assertEqualTestTemplate "parseAExpr successes with" " a 23aa " (parse parseAExpr) [(EVar "a","23aa ")]

testParseDef1 :: TestTree
testParseDef1 = assertEqualTestTemplate "parseDef successes with" "x = Pack{121,0}" (parse parseDef)  [(("x", EConstr 121 0),"")]

testParseDef2 :: TestTree
testParseDef2 = assertEqualTestTemplate "parseDef successes with" "var = 3" (parse parseDef)  [(("var", ENum 3),"")]

testParseDef3 :: TestTree
testParseDef3 = assertEqualTestTemplate "parseDef fails with" "vv  3" (parse parseDef)  []

testParseDef4 :: TestTree
testParseDef4 = assertEqualTestTemplate "parseDef fails with" "vv = " (parse parseDef)  []

testParseAlt1 :: TestTree
testParseAlt1 = assertEqualTestTemplate "parseAlt successes with" "<1> x y -> Pack{121,0}" (parse parseAlt)  [((1, ["x", "y"],  EConstr 121 0),"")]

testParseAlt2 :: TestTree
testParseAlt2 = assertEqualTestTemplate "parseAlt successes with" "<1>  -> Pack{121,0}" (parse parseAlt)  [((1, [],  EConstr 121 0),"")]

testParseAlt3 :: TestTree
testParseAlt3 = assertEqualTestTemplate "parseAlt fails with" "<1> x y  Pack{121,0}" (parse parseAlt)  []

testParseAlt4 :: TestTree
testParseAlt4 = assertEqualTestTemplate "parseAlt fails with" "<a> x y -> Pack{121,0}" (parse parseAlt)  []

testParseAlt5 :: TestTree
testParseAlt5 = assertEqualTestTemplate "parseAlt fails with" "<1> x y -> " (parse parseAlt)  []

testParseList1 :: TestTree
testParseList1 = assertEqualTestTemplate "parseList of parseAlt success with" "<1> x y -> Pack{121,0}" (parse (parseList parseAlt ";"))  [([(1, ["x","y"], EConstr 121 0)],"")]

testParseList2 :: TestTree
testParseList2 = assertEqualTestTemplate "parseList of parseAlt success with" "<1> x y -> Pack{121,0}; <2>  -> 1;  <3> x -> test" (parse (parseList parseAlt ";"))  [([(1, ["x","y"], EConstr 121 0), (2, [], ENum 1), (3, ["x"], EVar "test")],"")]

testParseList3 :: TestTree
testParseList3 = assertEqualTestTemplate "parseList of parseDef success with" "x = Pack{121,0}" (parse (parseList parseDef ";")) [([("x", EConstr 121 0)],"")]

testParseList4 :: TestTree
testParseList4 = assertEqualTestTemplate "parseList of parseDef success with" "x = Pack{121,0} ; y = 12" (parse (parseList parseDef ";"))  [([("x", EConstr 121 0), ("y", ENum 12)],"")]