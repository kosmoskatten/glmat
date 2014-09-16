module UnitTests (suite) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit

suite :: [Test.Framework.Test]
suite = [ testGroup "Matrix4 tests"
          [ testCase "Identity matrix 4x4" identityMatrix4x4Test
          ]
        ]

identityMatrix4x4Test :: Assertion
identityMatrix4x4Test = assertBool "Should be true" False
