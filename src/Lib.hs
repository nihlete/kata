module Lib () where

import Control.Applicative
  ( Alternative (empty, some, (<|>)),
    Applicative (liftA2),
  )
import Data.Bifunctor (Bifunctor (first))
import Data.Char (isDigit)

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

-- todo possibly flatten down Number datatype into Expr
data Number = Integer Int | Floating Double
  deriving (Eq, Show)

data Expr
  = ConstExpr Number
  | AddExpr Expr Expr
  | SubExpr Expr Expr
  | MulExpr Expr Expr
  | DivExpr Expr Expr
  | NegExpr Expr
  deriving (Eq, Show)

chainl1 = undefined

parseExpr :: Parser Expr
parseExpr = _ termParser op
  where
    op =
      AddExpr <$ charParser (== '+')
        <|> SubExpr <$ charParser (== '-')
        <|> MulExpr <$ charParser (== '*')
        <|> DivExpr <$ charParser (== '/')

termParser :: Parser Expr
termParser = undefined

parens :: Parser a -> Parser a
parens p = charParser (== '(') *> p <* charParser (== ')')

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

-- evaluateExpr :: Expr -> Double
-- evaluateExpr (ConstExpr (Floating x)) = x
-- evaluateExpr (ConstExpr (Integer x)) = fromIntegral x
-- evaluateExpr (NegExpr e) = -(evaluateExpr e)
-- evaluateExpr (ParenExpr e) = evaluateExpr e
-- evaluateExpr (BinaryExpr a Add b) = evaluateExpr a + evaluateExpr b
-- evaluateExpr (BinaryExpr a Sub b) = evaluateExpr a - evaluateExpr b
-- evaluateExpr (BinaryExpr a Mul b) = evaluateExpr a * evaluateExpr b
-- evaluateExpr (BinaryExpr a Div b) = evaluateExpr a / evaluateExpr b

-- printExpr :: Expr -> String
-- printExpr (ConstExpr (Floating x)) = show x
-- printExpr (ConstExpr (Integer x)) = show x
-- printExpr (NegExpr e) = "-" ++ printExpr e
-- printExpr (ParenExpr e) = "(" ++ printExpr e ++ ")"
-- printExpr (BinaryExpr a Add b) = printExpr a ++ "+" ++ printExpr b
-- printExpr (BinaryExpr a Sub b) = printExpr a ++ "-" ++ printExpr b
-- printExpr (BinaryExpr a Mul b) = printExpr a ++ "*" ++ printExpr b
-- printExpr (BinaryExpr a Div b) = printExpr a ++ "/" ++ printExpr b

-- calc :: String -> Double
-- calc s = evaluateExpr . fst . head $ runParser exprParser s
