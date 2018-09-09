# core-language-parser

stack build
stack test
stack build --exec core-language-parser-exe
stack exec core-language-parser-exe -- "res/input.txt"
