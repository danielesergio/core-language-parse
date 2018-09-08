module Main where

import Lib
import Read

main :: IO ()
path = "res/input.txt"
main = readF path >>= putStrLn