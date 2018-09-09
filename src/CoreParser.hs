module CoreParser(
          alphanumWithUnderScore,
          parseAExpr,
          parseDef,
          parseAlt,
          parseList
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
parseVar = token parseVar_

parseVar_ :: Parser (Name)
parseVar_ =  do x  <- letter
                xs <- many alphanumWithUnderScore
                if isKey (x:xs) then empty else return (x:xs)

parserAExprNumber :: Parser (Expr Name)
parserAExprNumber = do n <- natural
                       return (ENum n)

alphanumWithUnderScore :: Parser Char
alphanumWithUnderScore = alphanum <|> sat (== '_')


-- start parseDef
parseDef :: Parser (Def Name)
parseDef =  do var <- parseVar
               symbol "="
               expr <- parseExpr
               return (var, expr)

-- end parseDef

-- start parseAlt
parseAlt :: Parser (Alter Name)
parseAlt = do symbol "<"
              val <- natural
              symbol ">"
              vars <- many parseVar
              symbol "->"
              expr <- parseExpr
              return (val, vars, expr)

-- end  parseAlt

-- start parseExpr
parseExpr :: Parser (Expr Name)
parseExpr = parseAExpr
-- end  parseExpr

parseList :: Parser a -> String -> Parser [a]
parseList p separator = do x <- p
                           do symbol separator
                              xs <- parseList p separator
                              return (x:xs)
                              <|> return([x])

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
