module Lib () where

import Data.Foldable (Foldable (foldl'))

data Answer = No | Almost | Yes deriving (Show, Read, Eq, Ord)

isInteresting :: Integer -> [Integer] -> Answer
isInteresting x xs
  | x == 98 = Almost
  | x == 99 = Almost
  | x < 100 = No
  | otherwise = maximum $ map (isPhrase x) (xs ++ ys) ++ [f x]
  where
    ys = generatePhrases (len x)
    f x
      | isPalindrome x || isIncreasing x || isDecreasing x = Yes
      | isPalindrome (x + 1) || isIncreasing (x + 1) || isDecreasing (x + 1) = Almost
      | isPalindrome (x + 2) || isIncreasing (x + 2) || isDecreasing (x + 2) = Almost
      | otherwise = No

isPhrase :: Integer -> Integer -> Answer
isPhrase x y = case y - x of
  0 -> Yes
  1 -> Almost
  2 -> Almost
  _ -> No

digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

fromDigs :: [Integer] -> Integer
fromDigs = foldl' (\x d -> x * 10 + d) 0

len :: (Num p, Integral t) => t -> p
len 0 = 0
len x = 1 + len (x `div` 10)

generatePhrases :: Integer -> [Integer]
generatePhrases n =
  [d * 10 ^ (n - 1) | d <- [1 .. 9]]
    ++ [d * ones | d <- [1 .. 9]]
    ++ [10 ^ n, 10 ^ n + 1]
  where
    ones = fromDigs $ replicate (fromIntegral n) 1

isIncreasing :: Integer -> Bool
isIncreasing x = helper d ds
  where
    (d : ds) = digs x
    helper y [] = True
    helper a (b : ys) = (a + 1) `mod` 10 == b && helper b ys

isDecreasing :: Integer -> Bool
isDecreasing x = helper d ds
  where
    (d : ds) = digs x
    helper y [] = True
    helper a (b : ys) = (a - 1) == b && helper b ys

isPalindrome :: Integer -> Bool
isPalindrome x = x == y
  where
    y = fromDigs . reverse . digs $ x