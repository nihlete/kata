module Lib
  ( someFunc,
  )
where

import Control.Applicative
import Data.Bifunctor

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

someFunc :: IO ()
someFunc = putStrLn "someFunc"
