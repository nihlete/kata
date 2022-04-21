module Lib () where

import Control.Applicative
  ( Alternative (empty, many, some, (<|>)),
    Applicative (liftA2),
  )
import Data.Bifunctor (Bifunctor (first))
import Data.Char (isDigit, isSpace)

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

chainl1 :: Parser Expr -> Parser (Expr -> Expr -> Expr) -> Parser Expr
chainl1 p pop = foldl comb <$> p <*> rest
  where
    comb x (op, y) = x `op` y
    rest = many ((,) <$> pop <*> p)

exprParser :: Parser Expr
exprParser = termParser `chainl1` addOp

termParser :: Parser Expr
termParser = factorParser `chainl1` mulOp

factorParser :: Parser Expr
factorParser = parens exprParser <|> constParser <|> minus (parens exprParser <|> constParser <|> minus exprParser)

addOp :: Parser (Expr -> Expr -> Expr)
addOp = AddExpr <$ charParser (== '+') <|> SubExpr <$ charParser (== '-')

mulOp :: Parser (Expr -> Expr -> Expr)
mulOp = MulExpr <$ charParser (== '*') <|> DivExpr <$ charParser (== '/')

parens :: Parser a -> Parser a
parens p = charParser (== '(') *> p <* charParser (== ')')

minus :: Parser Expr -> Parser Expr
minus p = NegExpr <$> (charParser (== '-') *> p)

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

evaluateExpr :: Expr -> Double
evaluateExpr (ConstExpr (Floating x)) = x
evaluateExpr (ConstExpr (Integer x)) = fromIntegral x
evaluateExpr (NegExpr e) = -(evaluateExpr e)
evaluateExpr (AddExpr a b) = evaluateExpr a + evaluateExpr b
evaluateExpr (SubExpr a b) = evaluateExpr a - evaluateExpr b
evaluateExpr (MulExpr a b) = evaluateExpr a * evaluateExpr b
evaluateExpr (DivExpr a b) = evaluateExpr a / evaluateExpr b

calc :: String -> Double
calc s = evaluateExpr . fst . head $ runParser exprParser $ filter (not . isSpace) s
