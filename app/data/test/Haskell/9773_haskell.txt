module Main where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import Solutions.Problem001 as P1

tests = TestList [
    TestCase (assertEqual "Test 1a" 23 (P1.solution 10)),
    TestCase (assertEqual "Test 1b" 233168 (P1.solution 1000))
    ]

main = defaultMain $ hUnitTestToTests tests
