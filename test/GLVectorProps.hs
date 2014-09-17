module GLVectorProps
       ( suite
       ) where

import Graphics.Math.GLVector (GLVector (..), sub)

import Control.Applicative ((<$>), (<*>))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

instance (Arbitrary a) => Arbitrary (GLVector a) where
  arbitrary = GLVector <$> arbitrary <*> arbitrary <*> arbitrary

suite :: [Test.Framework.Test]
suite = [ testGroup "GLVector properties"
          [ testProperty "Vector subtracted with itself become 0 0 0"
                         subtractProp
          ]
        ]

subtractProp :: GLVector Float -> Bool
subtractProp vec = GLVector 0 0 0 == vec `sub` vec


