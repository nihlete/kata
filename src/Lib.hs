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
  | NegExpr Expr
  | BinaryExpr Expr Op Expr
  | ParenExpr Expr
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
  a <|> b = Parser (\s -> runParser a s ++ runParser b s)

parseChar :: (Char -> Bool) -> Parser Char
parseChar pred = Parser g
  where
    g "" = []
    g (c : ss)
      | pred c = [(c, ss)]
      | otherwise = []

parseInt :: Parser Int
parseInt = Parser g
  where
    g s = do
      let res = runParser (some (parseChar validChar)) s
      if null res then empty else pure (first read (head res))
    validChar c = isDigit c || (c == '.')

parseDouble :: Parser Double
parseDouble = Parser g
  where
    g s = do
      let res = runParser (some (parseChar validChar)) s
      if null res then empty else pure (first read (head res))
    validChar c = isDigit c || (c == '.')

constParser :: Parser Expr
constParser = ConstExpr . Integer <$> parseInt <|> ConstExpr . Floating <$> parseDouble


-- negParser :: Parser Expr
-- negParser = parseChar (== '-') *> (NegExpr <$> exprParser)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
