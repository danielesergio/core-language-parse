module CoreParser(
          alphanumWithUnderScore,
          parseAExpr
) where

import Lib
import Model
import Control.Applicative

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



--parseExpr :: Parser (Expr Name)
--parseAExpr :: Parser (Expr Name)
--parseDef :: Parser (Def Name)
--parseAlt :: Parser (Alter Name)



character :: Char -> Parser Char
character x = token (Lib.char x)
--

-- start parseAExpr
parseAExpr :: Parser (Expr Name)
parseAExpr = token parseAExpr_

parseAExpr_ :: Parser (Expr Name)
parseAExpr_ = parseExprIntoBrackets <|> parsePack <|> parserAExprVar <|> parserAExprNumber

parsePack :: Parser (Expr Name)
parsePack = do string "Pack"
               symbol "{"
               x <- natural
               symbol ","
               y <- natural
               symbol "}"
               return (EConstr x y)

parseExprIntoBrackets :: Parser (Expr Name)
parseExprIntoBrackets = do symbol "("
                           expr <- parseExpr
                           symbol ")"
                           return expr

-- todo check that prevents var is a key (ex Pack) respect langauge{--} grammar
parserAExprVar :: Parser (Expr Name)
parserAExprVar =  do x <- parseVar
                     return (EVar x)
-- start parseAExpr
parseVar :: Parser (Name)
parseVar =  do x <- letter
               xs <-many alphanumWithUnderScore
               if isKey (x:xs) then empty else return (x:xs)

parserAExprNumber :: Parser (Expr Name)
parserAExprNumber = do n <- nat
                       return (ENum n)

alphanumWithUnderScore :: Parser Char
alphanumWithUnderScore = alphanum <|> sat (== '_')


-- start parseDef

-- end parseDef

-- start parseAlt

-- end  parseAlt

-- start parseExpr
parseExpr :: Parser (Expr Name)
parseExpr = parseAExpr
-- end  parseExpr

isKey :: String -> Bool
isKey = \x -> elem x ["Pack"]
{-
expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
             <|> return t
             -}
