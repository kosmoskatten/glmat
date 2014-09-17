module GLVectorTests
       ( suite
       ) where

import Graphics.Math.GLVector
  ( GLVector (..)
  , magnitude
  , normalize
  , cross
  )

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit

suite :: [Test.Framework.Test]
suite = [ testGroup "GLVector tests"
          [ testCase "Vector magnitude" vectorMagnitudeTest
          , testCase "Normalize vector" normalizeVectorTest
          , testCase "Cross product"    crossProductTest
          ]
        ]

vectorMagnitudeTest :: Assertion
vectorMagnitudeTest =
  assertEqual "Should be equal" (sqrt $ 300 :: Float)
                                (magnitude $ GLVector 10 10 10)

normalizeVectorTest :: Assertion
normalizeVectorTest = do
  let x   = 10
      y   = 20
      z   = 30
      vec = GLVector x y z :: GLVector Float
      mag = magnitude vec
  assertEqual "Should be equal" (GLVector (x / mag) (y / mag) (z / mag))
                                (normalize vec)

crossProductTest :: Assertion
crossProductTest = do
  let a = GLVector 2 3 4 :: GLVector Float
      b = GLVector 5 6 7
  assertEqual "Should be equal" (GLVector (-3) 6 (-3)) (a `cross` b)
