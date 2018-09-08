module CoreParser(
          alphanumWithUnderScore
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



-- use symbol instead of character
--character :: Char -> Parser Char
--character x = token (Lib.char x)
--
--parseAExpr :: Parser (Expr String)
--parseAExpr = do x <- character
--                xs <-some alphanumWithUnderScore
--                return (Evar x:xs)
--
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
