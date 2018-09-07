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
