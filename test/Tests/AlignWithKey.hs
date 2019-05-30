module Tests.AlignWithKey (alignWithKeyProps) where

import Test.QuickCheck           ((===), once)
import Test.Tasty                (TestTree, testGroup)
import Test.Tasty.QuickCheck     (testProperty)

import Data.These
import Data.Align.Indexed
import Data.Align.Key

alignWithKeyProps :: TestTree
alignWithKeyProps = testGroup "AlignWithKey / AlignWithIndex"
    [ testProperty "example" $ once exampleK
    , testProperty "example" $ once exampleI
    ]
  where
    exampleK = alignWithKey (,) "foo" "quux" === exampleV
    exampleI = ialign (,) "foo" "quux" === exampleV

    exampleV =
        [ (0, These 'f' 'q')
        , (1, These 'o' 'u')
        , (2, These 'o' 'u')
        , (3, That 'x')
        ]
