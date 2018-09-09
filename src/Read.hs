module Read(
        readFileAndParse
    ) where

import System.IO
import Model(Program, Name)
import CoreParser
import Model
import Lib
import Control.Applicative

readFileAndParse :: String -> IO (Program Name)
readFileAndParse path = do inp <- readF path
                           return (comp (parse parseProg inp)) --here you call parseProg

comp :: [(Program Name, Name)] -> Program Name
comp []       = error "no parse"
comp [(e,[])] = e
comp [(_,a)]  = error ("doesn't use all input"++ a)


readF::FilePath -> IO String
readF path = do inh<- (openFile path) ReadMode
                prog<- readloop inh
                hClose inh
                return prog

readloop inh = do ineof <- hIsEOF inh
                  if ineof
                       then return []
                       else do
                              x  <- hGetLine inh
                              xs <- readloop inh
                              return (x++xs)


parseProg :: Parser (Program Name)
parseProg = do p <- parseScDef
               do character ';'
                  ps <- parseProg
                  return (p:ps)
                  <|> return [p]

parseScDef :: Parser (ScDefn Name)
parseScDef = do v <- parseVar
                pf <- many parseVar
                character '='
                body <- parseExpr -- call to parseExpr
                return (v, pf, body)