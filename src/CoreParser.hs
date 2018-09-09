module CoreParser(
          alphanumWithUnderScore,
          parseAExpr
) where

import Lib
import Model
import Control.Applicative
{-
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
-}



--parseExpr :: Parser (Expr Name)
--parseAExpr :: Parser (String Name)
--parseDef :: Parser (Def Name)
--parseAlt :: Parser (Alter Name)



--character :: Char -> Parser Char
--character x = token (Lib.char x)
--

parseAExpr :: Parser (Expr String)
parseAExpr = token parseAExpr_

parseAExpr_ :: Parser (Expr String)
parseAExpr_ = parsePack <|> parserAExprVar <|> parserAExprNumber

parsePack :: Parser (Expr String)
parsePack = do string "Pack"
               symbol "{"
               x <- natural
               symbol ","
               y <- natural
               symbol "}"
               return (EConstr x y)


parserAExprVar :: Parser (Expr String)
parserAExprVar =  do x <- letter
                     xs <-some alphanumWithUnderScore
                     return (Evar (x:xs))

parserAExprNumber :: Parser (Expr String)
parserAExprNumber = do n <- nat
                       return (ENum n)

alphanumWithUnderScore :: Parser Char
alphanumWithUnderScore = alphanum <|> sat (== '_')

{-
expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
             <|> return t
             -}
