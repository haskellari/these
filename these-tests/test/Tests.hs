module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import Tests.Crosswalk
import Tests.Semialign
import Tests.SemialignWithIndex
import Tests.These

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ theseProps
    , alignProps
    , alignWithKeyProps
    , crosswalkProps
    ]
