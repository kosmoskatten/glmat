module UnitTests (suite) where

import Graphics.Math.GLMatrix (Matrix4 (..), identity4)

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit

suite :: [Test.Framework.Test]
suite = [ testGroup "Matrix4 tests"
          [ testCase "Identity matrix 4x4" identityMatrix4x4Test
          ]
        ]

identityMatrix4x4Test :: Assertion
identityMatrix4x4Test = 
    assertEqual "Should be equal" (Matrix4 1 0 0 0
                                           0 1 0 0
                                           0 0 1 0
                                           0 0 0 1)
                                  (identity4 :: Matrix4 Int)
