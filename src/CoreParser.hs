module CoreParser(
          alphanumWithUnderScore,
          parseAExpr,
          parseDef,
          parseAlt,
          parseList,
          parseLet,
          parseIsRec,
          parseCase,
          parseLambda,
          character,
          parseVar,
          parseExpr,
          parseExpr1,
          parseExpr2,
          parseExpr3,
          parseExpr4,
          parseExpr5,
          parseExpr6
) where

import Lib
import Model
import Control.Applicative
import Data.Char (isSymbol)


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
parseExpr = parseLet <|> parseCase <|> parseLambda <|> parseExpr1
-- end  parseExpr

parseList :: Parser a -> String -> Parser [a]
parseList p separator = do x <- p
                           do symbol separator
                              xs <- parseList p separator
                              return (x:xs)
                              <|> return([x])

parseIsRec :: Parser IsRec
parseIsRec = do x <- token (many letter)
                case x of
                  "let"    -> return (NonRecursive)
                  "letrec" -> return (Recursive)
                  _ -> empty

parseLet :: Parser (Expr Name)
parseLet = do isRec <- parseIsRec
              defs  <- parseList parseDef ";"
              symbol "in"
              expr  <- parseExpr
              return (ELet isRec defs expr)

parseCase :: Parser (Expr Name)
parseCase  = do symbol "case"
                expr <- parseExpr
                symbol "of"
                alts <- parseList parseAlt ";"
                return (ECase expr alts)

parseLambda :: Parser (Expr Name)
parseLambda = do symbol "\\"
                 vars <- some parseVar
                 symbol "."
                 expr <- parseExpr
                 return (ELam vars expr)

isKey :: String -> Bool
isKey = \x -> elem x ["Pack", "let", "letrec", "in", "case", "of"]

parseExprRightAssociative ::  Parser (Expr Name) ->Parser (Expr Name) -> Parser (Expr Name) -> Parser (Expr Name)
parseExprRightAssociative opp p1 p2 = do e1 <- p1
                                         do op <- opp
                                            e2 <- p2
                                            return (EAp (EAp op e1) e2)
                                          <|> return e1

parseRelopSymbol :: Parser (Expr Name)
parseRelopSymbol = do op <- token (many (sat isSymbol))
                      if elem op ["<","<=","==","~=",">=",">" ] then return(EVar op) else empty

parseOperationSymbol :: String -> Parser (Expr Name)
parseOperationSymbol op = do op_ <- symbol op
                             return (EVar op_)

parseExpr1 :: Parser (Expr Name)
parseExpr1 = parseExprRightAssociative (parseOperationSymbol "|") parseExpr2 parseExpr1

parseExpr2 :: Parser (Expr Name)
parseExpr2 = parseExprRightAssociative (parseOperationSymbol "&") parseExpr3 parseExpr2

parseExpr3 :: Parser (Expr Name)
parseExpr3 = parseExprRightAssociative parseRelopSymbol parseExpr4 parseExpr4

parseExpr4 :: Parser (Expr Name)
parseExpr4 = do e1 <- parseExpr5
                do op <- parseOperationSymbol "+"
                   e2 <- parseExpr4
                   return (EAp (EAp op e1) e2)
                  <|> do op <- parseOperationSymbol "-"
                         e2 <- parseExpr5
                         return (EAp (EAp op e1) e2)
                         <|> return e1

parseExpr5 :: Parser (Expr Name)
parseExpr5 = do e1 <- parseExpr6
                do op <- parseOperationSymbol "*"
                   e2 <- parseExpr5
                   return (EAp (EAp op e1) e2)
                  <|> do op <- parseOperationSymbol "/"
                         e2 <- parseExpr6
                         return (EAp (EAp op e1) e2)
                         <|> return e1


parseEAp :: [Expr Name] -> Expr Name
parseEAp xs = case xs of
                x:[] -> x
                xs -> EAp (parseEAp (init xs)) (last xs)

parseExpr6 :: Parser (Expr Name)
parseExpr6 = do aes <- some parseAExpr
                pure (parseEAp aes)
