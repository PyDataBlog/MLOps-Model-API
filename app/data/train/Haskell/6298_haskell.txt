module SortTest where

import Util.Sort
import Test.HUnit

tests = test [ "test1" ~: "quicksort [0]," ~: [0] ~=? (quicksort [0]),
               "test2" ~: "quicksort [1,0,2]," ~: [0,1,2] ~=? (quicksort [1,0,2]) ]

-- test1 = TestCase (assertEqual "quicksort [0]," [0] (quicksort [0]))
-- test2 = TestCase (assertEqual "quicksort [1,0,2]," [0,1,2] (quicksort [1,0,2]))
-- tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]
