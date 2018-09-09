module Main where

import System.Environment
import Lib
import Read
import Model

main :: IO ()
main =  do args <- getArgs
           readFileAndParse (if (length args > 0) then head args else "res/input.txt") >>= print

