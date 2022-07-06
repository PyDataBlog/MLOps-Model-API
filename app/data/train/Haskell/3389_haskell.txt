-- From Thinking Functionally with Haskell
-- One man went to mow
-- Went to mow a meadow
-- One man and his dog
-- Went to mow a meadow
-- Two men went to mow
-- Went to mow a meadow
-- Two men, one man and his dog
-- Went to mow a meadow
-- Three men went to mow
-- Went to mow a meadow
-- Three men, two men, one man and his dog
-- Went to mow a meadow

units :: [String]
units = ["zero","one","two","three","four","five","six","seven","eight","nine"]

convert1 :: Int -> String
convert1 n = units!!n

convert = convert1

men :: Int -> String
men n
  | n <= 1      = convert n ++ " man"
  | otherwise   = convert n ++ " men"

s1 :: Int -> String
s1 n = men n ++ " went to mow\n"

s2 :: Int -> String
s2 n = "Went to mow a meadow\n"

s3 :: Int -> String
s3 n
  | n <= 1      = men n ++ " and his dog\n"
  | otherwise  = men n ++ ", " ++ s3 (n - 1)

s4 :: Int -> String
s4 = s2

song :: Int -> String
song n
  | n == 0    = ""
  | n <10     = s1 n ++ s2 n ++ s3 n ++ s4 n
  | otherwise = ""
