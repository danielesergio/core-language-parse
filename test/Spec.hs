module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Lib
import Model
import Parser
import Text.Printf

main :: IO ()
main = do
  defaultMain (testGroup "Our Library Tests"[
    testAlphanumWithUnderScore1, testAlphanumWithUnderScore2, testAlphanumWithUnderScore3,
    testParseAExpr1, testParseAExpr2, testParseAExpr3, testParseAExpr4, testParseAExpr5, testParseAExpr6, testParseAExpr7,
    testParseAExpr8, testParseAExpr9, testParseAExpr10, testParseAExpr11,
    testParseDef1, testParseDef2, testParseDef3, testParseDef4, testParseDef5,
    testParseAlt1, testParseAlt2, testParseAlt3, testParseAlt4, testParseAlt5,
    testParseList1, testParseList2, testParseList3, testParseList4,
    testParseIsRec1, testParseIsRec2, testParseIsRec3, testParseIsRec4,
    testParseLet1, testParseLet2, testParseLet3, testParseLet4, testParseLet5, testParseLet6,
    testParseCase1, testParseCase2, testParseCase3, testParseCase4, testParseCase5, testParseCase6, testParseCase7,
    testParseLambda1, testParseLambda2, testParseLambda3, testParseLambda4, testParseLambda5, testParseLambda6,
    testParseExpr1_1, testParseExpr1_2, testParseExpr1_3, testParseExpr1_4, testParseExpr1_5, testParseExpr1_6, testParseExpr1_7,
    testParseExpr1_8, testParseExpr1_9, testParseExpr1_10, testParseExpr1_11, testParseExpr1_12, testParseExpr1_13,
    testParseExpr1_14, testParseExpr1_15, testParseExpr1_16, testParseExpr1_17, testParseExpr1_18, testParseExpr1_19,
    testParseExpr1_20, testParseExpr1_21, testParseExpr1_22, testParseExpr1_23, testParseExpr1_24, testParseExpr1_25,
    testParseExpr1_26, testParseExpr1_27, testParseExpr1_28, testParseExpr1_29, testParseExpr1_30, testParseExpr1_31, testParseExpr1_32,
    testParseExpr1_33, testParseExpr1_34, testParseExpr1_35, testParseExpr1_36, testParseExpr1_37, testParseExpr1_38, testParseExpr1_39,
    testParseExpr1_40, testParseExpr1_41, testParseExpr1_42, testParseExpr1_43, testParseExpr1_44, testParseExpr1_45, testParseExpr1_46,
    testParseExpr1_47, testParseExpr1_48, testParseExpr1_49
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

testParseDef5 :: TestTree
testParseDef5 = assertEqualTestTemplate "parseDef fails with" " = 3" (parse parseDef)  []

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

testParseIsRec1 :: TestTree
testParseIsRec1 = assertEqualTestTemplate "parseIsRec success with" " let " (parse parseIsRec)  [(NonRecursive,"")]

testParseIsRec2 :: TestTree
testParseIsRec2 = assertEqualTestTemplate "parseIsRec success with" " letrec " (parse parseIsRec)  [(Recursive,"")]

testParseIsRec3 :: TestTree
testParseIsRec3 = assertEqualTestTemplate "parseIsRec fails with" " letarec " (parse parseIsRec)  []

testParseIsRec4 :: TestTree
testParseIsRec4 = assertEqualTestTemplate "parseIsRec fails with" "  " (parse parseIsRec)  []

testParseLet1 :: TestTree
testParseLet1 = assertEqualTestTemplate "parseLet success with" "let x = Pack{121,0} ; y = 12 in z" (parse parseLet)  [(ELet NonRecursive [("x", EConstr 121 0), ("y", ENum 12)] (EVar "z"),"")]

testParseLet2 :: TestTree
testParseLet2 = assertEqualTestTemplate "parseLet success with" "let x = Pack{121,0} in z" (parse parseLet)  [(ELet NonRecursive [("x", EConstr 121 0)] (EVar "z"),"")]

testParseLet3 :: TestTree
testParseLet3 = assertEqualTestTemplate "parseLet fails with" "let x = Pack{121,0}; in z" (parse parseLet)  []

testParseLet4 :: TestTree
testParseLet4 = assertEqualTestTemplate "parseLet fails with" "let in z" (parse parseLet)  []

testParseLet5 :: TestTree
testParseLet5 = assertEqualTestTemplate "parseLet fails with" "let x = Pack{121,0}; adasd in z" (parse parseLet)  []

testParseLet6 :: TestTree
testParseLet6 = assertEqualTestTemplate "parseLet fails with" "let x = Pack{121,0} z" (parse parseLet)  []

testParseCase1 :: TestTree
testParseCase1 = assertEqualTestTemplate "parseCase success with" "case 1 of <1> x y -> Pack{121,0}; <2>  -> 1;  <3> x -> test" (parse parseCase)  [(ECase (ENum 1) [(1, ["x","y"], EConstr 121 0), (2, [], ENum 1), (3, ["x"], EVar "test")], "")]

testParseCase2 :: TestTree
testParseCase2 = assertEqualTestTemplate "parseCase success with" "case 1 of <1>  -> Pack{121,0}" (parse parseCase)  [(ECase (ENum 1) [(1, [], EConstr 121 0)], "")]

testParseCase3 :: TestTree
testParseCase3 = assertEqualTestTemplate "parseCase fails with" "case 1 of <1> x y -> " (parse parseCase)  []

testParseCase4 :: TestTree
testParseCase4 = assertEqualTestTemplate "parseCase fails with" "1 of <1> x y -> Pack{121,0}" (parse parseCase)  []

testParseCase5 :: TestTree
testParseCase5 = assertEqualTestTemplate "parseCase fails with" "case  of <1> x y -> Pack{121,0}" (parse parseCase)  []

testParseCase6 :: TestTree
testParseCase6 = assertEqualTestTemplate "parseCase fails with" "case 1 of  x y -> Pack{121,0}" (parse parseCase)  []

testParseCase7 :: TestTree
testParseCase7 = assertEqualTestTemplate "parseCase fails with" "case 1 of <1> x y  Pack{121,0}" (parse parseCase)  []

testParseLambda1 :: TestTree
testParseLambda1 = assertEqualTestTemplate "parseCase successes with" "\\ x1 x2 . Pack{121,0}" (parse parseLambda)  [( ELam ["x1", "x2"]  (EConstr 121 0), "")]

testParseLambda2 :: TestTree
testParseLambda2 = assertEqualTestTemplate "parseCase successes with" "\\ x1 . 1" (parse parseLambda)  [(ELam  ["x1"]  (ENum 1), "")]

testParseLambda3 :: TestTree
testParseLambda3 = assertEqualTestTemplate "parseCase fails with" " x1 . 1" (parse parseLambda)  []

testParseLambda4 :: TestTree
testParseLambda4 = assertEqualTestTemplate "parseCase fails with" "\\  . 1" (parse parseLambda)  []

testParseLambda5 :: TestTree
testParseLambda5 = assertEqualTestTemplate "parseCase fails with" "\\ x1   1" (parse parseLambda)  []

testParseLambda6 :: TestTree
testParseLambda6 = assertEqualTestTemplate "parseCase fails with" "\\ x1" (parse parseLambda)  []

-- TEST OPERATOR ASSOCIATIVITY
testParseExpr1_1 :: TestTree
testParseExpr1_1 = assertEqualTestTemplate "parseExpr1 successes with" " a | b | c" (parse parseExpr1)  [(EAp (EAp (EVar "|") (EVar "a")) (EAp (EAp (EVar "|") (EVar "b")) (EVar "c")), "")]

testParseExpr1_2 :: TestTree
testParseExpr1_2 = assertEqualTestTemplate "parseExpr1 successes with" " a & b & c" (parse parseExpr1)   [(EAp (EAp (EVar "&") (EVar "a")) (EAp (EAp (EVar "&") (EVar "b")) (EVar "c")), "")]

testParseExpr1_3 :: TestTree
testParseExpr1_3 = assertEqualTestTemplate "parseExpr1 successes with" " a == b == c" (parse parseExpr1)  [(EAp (EAp (EVar "==") (EVar "a")) (EVar "b"), "== c")]

testParseExpr1_4 :: TestTree
testParseExpr1_4 = assertEqualTestTemplate "parseExpr1 successes with" " a ~= b ~= c" (parse parseExpr1)  [(EAp (EAp (EVar "~=") (EVar "a")) (EVar "b"), "~= c")]

testParseExpr1_5 :: TestTree
testParseExpr1_5 = assertEqualTestTemplate "parseExpr1 successes with" " a >= b >= c" (parse parseExpr1)  [(EAp (EAp (EVar ">=") (EVar "a")) (EVar "b"), ">= c")]

testParseExpr1_6 :: TestTree
testParseExpr1_6 = assertEqualTestTemplate "parseExpr1 successes with" " a < b < c" (parse parseExpr1)  [(EAp (EAp (EVar "<") (EVar "a")) (EVar "b"), "< c")]

testParseExpr1_7 :: TestTree
testParseExpr1_7 = assertEqualTestTemplate "parseExpr1 successes with" " a > b > c" (parse parseExpr1)  [(EAp (EAp (EVar ">") (EVar "a")) (EVar "b"), "> c")]

testParseExpr1_8 :: TestTree
testParseExpr1_8 = assertEqualTestTemplate "parseExpr1 successes with" " a + b + c" (parse parseExpr1)  [(EAp (EAp (EVar "+") (EVar "a")) (EAp (EAp (EVar "+") (EVar "b")) (EVar "c")), "")]

testParseExpr1_9 :: TestTree
testParseExpr1_9 = assertEqualTestTemplate "parseExpr1 successes with" " a * b * c" (parse parseExpr1)  [(EAp (EAp (EVar "*") (EVar "a")) (EAp (EAp (EVar "*") (EVar "b")) (EVar "c")), "")]

testParseExpr1_10 :: TestTree
testParseExpr1_10 = assertEqualTestTemplate "parseExpr1 successes with" " a / b / d" (parse parseExpr1)  [(EAp (EAp (EVar "/") (EVar "a")) (EVar "b"), "/ d")]

testParseExpr1_11 :: TestTree
testParseExpr1_11 = assertEqualTestTemplate "parseExpr1 successes with" " a <= b <=c" (parse parseExpr1)  [(EAp (EAp (EVar "<=") (EVar "a")) (EVar "b"), "<=c")]

testParseExpr1_12 :: TestTree
testParseExpr1_12 = assertEqualTestTemplate "parseExpr1 successes with" " f g h " (parse parseExpr1)  [(EAp (EAp (EVar "f") (EVar "g")) (EVar "h"), "")]

-- TEST OPERATOR ASSOCIATIVITY

-- Application
testParseExpr1_13 :: TestTree
testParseExpr1_13 = assertEqualTestTemplate "parseExpr1 successes with" " f g | h " (parse parseExpr1)  [(EAp (EAp (EVar "|") (EAp (EVar "f") (EVar "g"))) (EVar "h") , "")]

testParseExpr1_14 :: TestTree
testParseExpr1_14 = assertEqualTestTemplate "parseExpr1 successes with" " f g + h " (parse parseExpr1)  [(EAp (EAp (EVar "+") (EAp (EVar "f") (EVar "g"))) (EVar "h") , "")]

testParseExpr1_15 :: TestTree
testParseExpr1_15 = assertEqualTestTemplate "parseExpr1 successes with" " f g - h " (parse parseExpr1)  [(EAp (EAp (EVar "-") (EAp (EVar "f") (EVar "g"))) (EVar "h") , "")]

testParseExpr1_16 :: TestTree
testParseExpr1_16 = assertEqualTestTemplate "parseExpr1 successes with" " f g * h " (parse parseExpr1)  [(EAp (EAp (EVar "*") (EAp (EVar "f") (EVar "g"))) (EVar "h") , "")]

testParseExpr1_17 :: TestTree
testParseExpr1_17 = assertEqualTestTemplate "parseExpr1 successes with" " f g / h " (parse parseExpr1)  [(EAp (EAp (EVar "/") (EAp (EVar "f") (EVar "g"))) (EVar "h") , "")]

testParseExpr1_18 :: TestTree
testParseExpr1_18 = assertEqualTestTemplate "parseExpr1 successes with" " f g == h " (parse parseExpr1)  [(EAp (EAp (EVar "==") (EAp (EVar "f") (EVar "g"))) (EVar "h") , "")]

testParseExpr1_19 :: TestTree
testParseExpr1_19 = assertEqualTestTemplate "parseExpr1 successes with" " f g >= h " (parse parseExpr1)  [(EAp (EAp (EVar ">=") (EAp (EVar "f") (EVar "g"))) (EVar "h") , "")]

testParseExpr1_20 :: TestTree
testParseExpr1_20 = assertEqualTestTemplate "parseExpr1 successes with" " f g <= h " (parse parseExpr1)  [(EAp (EAp (EVar "<=") (EAp (EVar "f") (EVar "g"))) (EVar "h") , "")]

testParseExpr1_21 :: TestTree
testParseExpr1_21 = assertEqualTestTemplate "parseExpr1 successes with" " f g < h " (parse parseExpr1)  [(EAp (EAp (EVar "<") (EAp (EVar "f") (EVar "g"))) (EVar "h") , "")]

testParseExpr1_22 :: TestTree
testParseExpr1_22 = assertEqualTestTemplate "parseExpr1 successes with" " f g > h " (parse parseExpr1)  [(EAp (EAp (EVar ">") (EAp (EVar "f") (EVar "g"))) (EVar "h") , "")]

testParseExpr1_23 :: TestTree
testParseExpr1_23 = assertEqualTestTemplate "parseExpr1 successes with" " f | g  h " (parse parseExpr1)  [(EAp (EAp (EVar "|") (EVar "f")) (EAp (EVar "g") (EVar "h")) , "")]

testParseExpr1_24 :: TestTree
testParseExpr1_24 = assertEqualTestTemplate "parseExpr1 successes with" " f + g h " (parse parseExpr1) [(EAp (EAp (EVar "+") (EVar "f")) (EAp (EVar "g") (EVar "h")) , "")]

testParseExpr1_25 :: TestTree
testParseExpr1_25 = assertEqualTestTemplate "parseExpr1 successes with" " f - g h " (parse parseExpr1) [(EAp (EAp (EVar "-") (EVar "f")) (EAp (EVar "g") (EVar "h")) , "")]

testParseExpr1_26 :: TestTree
testParseExpr1_26 = assertEqualTestTemplate "parseExpr1 successes with" " f * g h " (parse parseExpr1)  [(EAp (EAp (EVar "*") (EVar "f")) (EAp (EVar "g") (EVar "h")) , "")]

testParseExpr1_27 :: TestTree
testParseExpr1_27 = assertEqualTestTemplate "parseExpr1 successes with" " f / g h " (parse parseExpr1)  [(EAp (EAp (EVar "/") (EVar "f")) (EAp (EVar "g") (EVar "h")) , "")]

testParseExpr1_28 :: TestTree
testParseExpr1_28 = assertEqualTestTemplate "parseExpr1 successes with" " f == g h " (parse parseExpr1) [(EAp (EAp (EVar "==") (EVar "f")) (EAp (EVar "g") (EVar "h")) , "")]

testParseExpr1_29 :: TestTree
testParseExpr1_29 = assertEqualTestTemplate "parseExpr1 successes with" " f >= g h " (parse parseExpr1)  [(EAp (EAp (EVar ">=") (EVar "f")) (EAp (EVar "g") (EVar "h")) , "")]

testParseExpr1_30 :: TestTree
testParseExpr1_30 = assertEqualTestTemplate "parseExpr1 successes with" " f <= g h " (parse parseExpr1)  [(EAp (EAp (EVar "<=") (EVar "f")) (EAp (EVar "g") (EVar "h")) , "")]

testParseExpr1_31 :: TestTree
testParseExpr1_31 = assertEqualTestTemplate "parseExpr1 successes with" " f < g h " (parse parseExpr1)  [(EAp (EAp (EVar "<") (EVar "f")) (EAp (EVar "g") (EVar "h")) , "")]

testParseExpr1_32 :: TestTree
testParseExpr1_32 = assertEqualTestTemplate "parseExpr1 successes with" " f > g h " (parse parseExpr1)  [(EAp (EAp (EVar ">") (EVar "f")) (EAp (EVar "g") (EVar "h")) , "")]

-- *
testParseExpr1_33 :: TestTree
testParseExpr1_33 = assertEqualTestTemplate "parseExpr1 successes with" " f * g | h " (parse parseExpr1)  [(EAp (EAp (EVar "|") (EAp (EAp (EVar "*") (EVar "f")) (EVar "g"))) (EVar "h") , "")]

testParseExpr1_34 :: TestTree
testParseExpr1_34 = assertEqualTestTemplate "parseExpr1 successes with" " f * g + h " (parse parseExpr1)  [(EAp (EAp (EVar "+") (EAp (EAp (EVar "*") (EVar "f")) (EVar "g"))) (EVar "h") , "")]

testParseExpr1_35 :: TestTree
testParseExpr1_35 = assertEqualTestTemplate "parseExpr1 successes with" " f * g - h " (parse parseExpr1) [(EAp (EAp (EVar "-") (EAp (EAp (EVar "*") (EVar "f")) (EVar "g"))) (EVar "h") , "")]

testParseExpr1_36 :: TestTree
testParseExpr1_36 = assertEqualTestTemplate "parseExpr1 successes with" " f * g > h " (parse parseExpr1) [(EAp (EAp (EVar ">") (EAp (EAp (EVar "*") (EVar "f")) (EVar "g"))) (EVar "h") , "")]

testParseExpr1_37 :: TestTree
testParseExpr1_37 = assertEqualTestTemplate "parseExpr1 successes with" " f * g / h " (parse parseExpr1)  [(EAp (EAp (EVar "*") (EVar "f")) (EAp (EAp (EVar "/") (EVar "g")) (EVar "h")),"")]

testParseExpr1_38 :: TestTree
testParseExpr1_38 = assertEqualTestTemplate "parseExpr1 successes with" " f * g == h " (parse parseExpr1) [(EAp (EAp (EVar "==") (EAp (EAp (EVar "*") (EVar "f")) (EVar "g"))) (EVar "h") , "")]

testParseExpr1_39 :: TestTree
testParseExpr1_39 = assertEqualTestTemplate "parseExpr1 successes with" " f * g >= h " (parse parseExpr1) [(EAp (EAp (EVar ">=") (EAp (EAp (EVar "*") (EVar "f")) (EVar "g"))) (EVar "h") , "")]

testParseExpr1_40 :: TestTree
testParseExpr1_40 = assertEqualTestTemplate "parseExpr1 successes with" " f * g <= h " (parse parseExpr1)  [(EAp (EAp (EVar "<=") (EAp (EAp (EVar "*") (EVar "f")) (EVar "g"))) (EVar "h") , "")]

testParseExpr1_41 :: TestTree
testParseExpr1_41 = assertEqualTestTemplate "parseExpr1 successes with" " f * g < h " (parse parseExpr1)  [(EAp (EAp (EVar "<") (EAp (EAp (EVar "*") (EVar "f")) (EVar "g"))) (EVar "h") , "")]

testParseExpr1_42 :: TestTree
testParseExpr1_42 = assertEqualTestTemplate "parseExpr1 successes with" " f | g * h " (parse parseExpr1)  [(EAp (EAp (EVar"|") (EVar "f"))(EAp (EAp (EVar "*") (EVar "g")) (EVar "h")) , "")]

testParseExpr1_43 :: TestTree
testParseExpr1_43 = assertEqualTestTemplate "parseExpr1 successes with" " f + g * h " (parse parseExpr1)  [(EAp (EAp (EVar"+") (EVar "f"))(EAp (EAp (EVar "*") (EVar "g")) (EVar "h")) , "")]

testParseExpr1_44 :: TestTree
testParseExpr1_44 = assertEqualTestTemplate "parseExpr1 successes with" " f > g * h " (parse parseExpr1)  [(EAp (EAp (EVar">") (EVar "f"))(EAp (EAp (EVar "*") (EVar "g")) (EVar "h")) , "")]

testParseExpr1_45 :: TestTree
testParseExpr1_45 = assertEqualTestTemplate "parseExpr1 successes with" " f / g * h " (parse parseExpr1)   [(EAp (EAp (EVar "/") (EVar "f")) (EVar "g"),"* h ")]

testParseExpr1_46 :: TestTree
testParseExpr1_46 = assertEqualTestTemplate "parseExpr1 successes with" " f == g * h " (parse parseExpr1)  [(EAp (EAp (EVar"==") (EVar "f"))(EAp (EAp (EVar "*") (EVar "g")) (EVar "h")) , "")]

testParseExpr1_47 :: TestTree
testParseExpr1_47 = assertEqualTestTemplate "parseExpr1 successes with" " f >= g * h " (parse parseExpr1)  [(EAp (EAp (EVar">=") (EVar "f"))(EAp (EAp (EVar "*") (EVar "g")) (EVar "h")) , "")]

testParseExpr1_48 :: TestTree
testParseExpr1_48 = assertEqualTestTemplate "parseExpr1 successes with" " f <= g * h " (parse parseExpr1)  [(EAp (EAp (EVar"<=") (EVar "f"))(EAp (EAp (EVar "*") (EVar "g")) (EVar "h")) , "")]

testParseExpr1_49 :: TestTree
testParseExpr1_49 = assertEqualTestTemplate "parseExpr1 successes with" " f < g * h " (parse parseExpr1)  [(EAp (EAp (EVar"<") (EVar "f"))(EAp (EAp (EVar "*") (EVar "g")) (EVar "h")) , "")]

-- + - op & |
-- test alter /