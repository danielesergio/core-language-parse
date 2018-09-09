module Main where

import Lib
import Read
import Model

main :: IO ()
main =  readFileAndParse "res/input.txt" >>= print

