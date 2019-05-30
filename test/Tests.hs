module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import Tests.Align
import Tests.AlignWithKey
import Tests.Crosswalk
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
