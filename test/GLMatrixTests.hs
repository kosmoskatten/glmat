module GLMatrixTests (suite) where

import Graphics.Math.GLMat
  ( GLMatrix (..)
  , identity
  , translate
  , scale
  , perspective
  , transpose
  , (>*<)
  )

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit

suite :: [Test.Framework.Test]
suite = [ testGroup "GLMatrix tests"
          [ testCase "Identity matrix"  identityMatrixTest
          , testCase "Translate"        translateTest
          , testCase "Scale"            scaleTest
          , testCase "Perspective"      perspectiveTest
          , testCase "Transpose"        transposeTest
          , testCase "Multiply a >*< b" aMulBTest
          ]
        ]

identityMatrixTest :: Assertion
identityMatrixTest = 
    assertEqual "Should be equal" (GLMatrix 1 0 0 0
                                            0 1 0 0
                                            0 0 1 0
                                            0 0 0 1)
                                  (identity :: GLMatrix Int)

translateTest :: Assertion
translateTest =
  assertEqual "Should be equal" (GLMatrix 1 0 0 0
                                          0 1 0 0
                                          0 0 1 0
                                          1 2 3 1)
                                (translate 1 2 3 :: GLMatrix Int)

scaleTest :: Assertion
scaleTest =
  assertEqual "Should be equal" (GLMatrix 2 0 0 0
                                          0 3 0 0
                                          0 0 4 0
                                          0 0 0 1)
                                (scale 2 3 4 :: GLMatrix Int)

perspectiveTest :: Assertion
perspectiveTest = do
  let degToRad theta = theta * (pi / 180)
      cot      theta = 1 / (tan theta)
      fovy           = 45
      fovy'          = degToRad fovy
      aspect         = 4 / 3
      zNear          = 0.1
      zFar           = 100 :: Float
  assertEqual "Should be equal"
              (GLMatrix ((cot (fovy' / 2)) / aspect) 0 0 0
                        0 (cot (fovy' / 2)) 0 0
                        0 0 ((zFar + zNear) / (zNear - zFar)) ((2 * zFar * zNear) / (zNear - zFar))
                        0 0 (-1) 0)
              (transpose $ perspective fovy aspect zNear zFar)

transposeTest :: Assertion
transposeTest =
  assertEqual "Should be equal" matA (transpose matB)

aMulBTest :: Assertion
aMulBTest =
  assertEqual "Should be equal" (GLMatrix  4  8 12 16
                                           8 16 24 32
                                          12 24 36 48
                                          16 32 48 64)
                                (matA >*< matB)

matA :: GLMatrix Int
matA = GLMatrix 1 1 1 1
                2 2 2 2
                3 3 3 3
                4 4 4 4

matB :: GLMatrix Int
matB = GLMatrix 1 2 3 4
                1 2 3 4
                1 2 3 4
                1 2 3 4
