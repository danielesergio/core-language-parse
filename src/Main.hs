module Main where

import Control.Applicative
import Data.Char


stringToParse = "adfasdf"

main :: IO ()
main = do
  case (parse item stringToParse) of
    []      -> putStrLn "Faile"
    [(a,b)] -> putChar a
--     putStrLn a:""



newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp



-- basic parser
item :: Parser Char
item = P (\inp -> case inp of
  []     -> []
  (x:xs) -> [(x,xs)])


instance Functor Parser where
-- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P (\inp -> case parse p inp of
    []        -> []
    [(v,out)] -> [(g v, out)])


instance Applicative Parser where
-- pure :: a -> Parser a
  pure v = P (\inp -> [(v,inp)])

-- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\inp -> case parse pg inp of
    []        -> []
    [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\inp -> case parse p inp of
    []        -> []
    [(v,out)] -> parse (f v) out)

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\inp -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\inp -> case parse p inp of
    []        -> parse q inp
    [(v,out)] -> [(v,out)])

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

space :: Parser ()
space = do many (sat isSpace)
           return ()

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
      <|> nat
{-
three :: Parser (Char,Char)
three = do x <- item
  item
  z <- item
  return (x,z)

tree =  item >>= \x ->
  item >>= \_ ->
  item >>= \z ->
  return (x,z)

-}
