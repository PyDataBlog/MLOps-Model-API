{-# LANGUAGE TemplateHaskell #-}

module UnitTest.Util.HogeTest (testSuite) where

import Test.Framework (defaultMain, Test)
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH


main = defaultMain [testSuite]

testSuite :: Test
testSuite = $(testGroupGenerator)


prop_concat :: [[Int]] -> Bool
prop_concat xs = concat xs == foldr (++) [] xs
