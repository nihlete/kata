module Lib
  ( someFunc,
  )
where

import Control.Applicative
import Data.Bifunctor
import Data.Char

data Number = Integer Int | Floating Double
  deriving (Eq, Show)

data Op = Mult | Div | Add | Sub
  deriving (Eq, Show)

data Expr
  = ConstExpr Number
  | BinaryExpr Expr Op Expr
  | ParenExpr Expr
  | NegExpr Expr
  deriving (Eq, Show)

newtype Parser a = Parser {runParser :: String -> [(a, String)]}

instance Functor Parser where
  fmap f p = Parser g
    where
      g s = map (first f) $ runParser p s

instance Applicative Parser where
  pure x = Parser (\s -> [(x, s)])
  liftA2 f a b = Parser g
    where
      g s = do
        (x1, s1) <- runParser a s
        (x2, s2) <- runParser b s1
        pure (f x1 x2, s2)

instance Alternative Parser where
  empty = Parser (const [])
  a <|> b = Parser (\s -> take 1 $ runParser a s ++ runParser b s)

alt :: Parser a -> Parser a -> Parser a
a `alt` b = Parser (\s -> runParser a s ++ runParser b s)

parse :: String -> Parser a -> Maybe a
parse s (Parser p) = case p s of
  [(val, "")] -> Just val
  _ -> Nothing

skip :: (Char -> Bool) -> Parser ()
skip p = Parser (\s -> [((), dropWhile p s)])

charParser :: (Char -> Bool) -> Parser Char
charParser pred = Parser g
  where
    g "" = []
    g (c : ss)
      | pred c = [(c, ss)]
      | otherwise = []

intParser :: Parser Int
intParser = Parser g
  where
    g s = do
      let res = runParser (some (charParser isDigit)) s
      if null res then empty else pure (first read (head res))

doubleParser :: Parser Double
doubleParser = Parser g
  where
    g s = do
      let res = runParser (some (charParser validChar)) s
      if null res then empty else pure (first read (head res))
    validChar c = isDigit c || (c == '.')

constParser :: Parser Expr
constParser = ConstExpr <$> Parser g
  where
    g s = helper r1 r2
      where
        r1 = runParser intParser s
        r2 = runParser doubleParser s
        helper [(x, ss)] [] = [(Integer x, ss)]
        helper [] [(x, ss)] = [(Floating x, ss)]
        helper [(x1, s1)] [(x2, s2)] = if length s2 < length s1 then [(Floating x2, s2)] else [(Integer x1, s1)]
        helper _ _ = []

opParser :: Parser Op
opParser = Parser g
  where
    g s = case result of
      [('+', ss)] -> [(Add, ss)]
      [('-', ss)] -> [(Sub, ss)]
      [('*', ss)] -> [(Mult, ss)]
      [('/', ss)] -> [(Div, ss)]
      _ -> []
      where
        result = runParser (charParser (== '+') <|> charParser (== '-') <|> charParser (== '*') <|> charParser (== '/')) s

binaryParser :: Parser Expr
binaryParser =
  BinaryExpr
    <$> (negParser <|> parenParser <|> constParser)
    <*> (skip (== ' ') *> opParser <* skip (== ' '))
    <*> exprParser

parenParser :: Parser Expr
parenParser = ParenExpr <$> (charParser (== '(') *> exprParser <* charParser (== ')'))

negParser :: Parser Expr
negParser = NegExpr <$> (charParser (== '-') *> exprParser)

exprParser :: Parser Expr
exprParser = negParser <|> parenParser <|> binaryParser <|> constParser


-- todo: 
-- ghci> evaluateExpr  . fst . head $ runParser binaryParser "2 * 5 + 1 "
-- 12.0
evaluateExpr :: Expr -> Double
evaluateExpr (ConstExpr (Floating x)) = x
evaluateExpr (ConstExpr (Integer x)) = fromIntegral x
evaluateExpr (NegExpr e) = -(evaluateExpr e)
evaluateExpr (ParenExpr e) = evaluateExpr e
evaluateExpr (BinaryExpr a Add b) = evaluateExpr a + evaluateExpr b
evaluateExpr (BinaryExpr a Sub b) = evaluateExpr a - evaluateExpr b
evaluateExpr (BinaryExpr a Mult b) = evaluateExpr a * evaluateExpr b
evaluateExpr (BinaryExpr a Div b) = evaluateExpr a / evaluateExpr b

someFunc :: IO ()
someFunc = putStrLn "someFunc"
