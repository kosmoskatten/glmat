module GLMatrixProps
       ( suite
       ) where

import Graphics.Math.GLMatrix (GLMatrix (..), identity, transpose, (>*<))

import Control.Applicative ((<$>), (<*>))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

instance (Arbitrary a) => Arbitrary (GLMatrix a) where
  arbitrary = GLMatrix <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                       <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                       <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                       <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

suite :: [Test.Framework.Test]
suite = [ testGroup "GLMatrix properties"
          [ testProperty "Transpose twice shall give original"
                         transposeTwiceProp
          , testProperty "identity >*< mat == mat"
                         identityMulMatProp
          , testProperty "mat >*< identity == mat"
                         matMulIdentityProp
          ]
        ]

transposeTwiceProp :: GLMatrix Int -> Bool
transposeTwiceProp mat = mat == (transpose $ transpose mat)

identityMulMatProp :: GLMatrix Int -> Bool
identityMulMatProp mat = mat == identity >*< mat

matMulIdentityProp :: GLMatrix Int -> Bool
matMulIdentityProp mat = mat == mat >*< identity

