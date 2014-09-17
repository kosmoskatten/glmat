module Graphics.Math.GLVector
       ( GLVector (..)
       , magnitude
       , normalize
       , cross
       , sub
       ) where

-- | A three dimensional vector.
data GLVector a = GLVector !a !a !a
  deriving (Eq, Show)

magnitude :: Floating a => GLVector a -> a
magnitude (GLVector x y z) = sqrt $ (x * x) + (y * y) + (z * z)

normalize :: Floating a => GLVector a -> GLVector a
normalize vec@(GLVector x y z) =
  let magnitude' = magnitude vec
  in GLVector (x / magnitude') (y / magnitude') (z / magnitude')

sub :: Floating a => GLVector a -> GLVector a -> GLVector a
sub (GLVector x1 y1 z1) (GLVector x2 y2 z2) =
  GLVector (x1 - x2) (y1 - y2) (z1 - z2)

cross :: Floating a => GLVector a -> GLVector a -> GLVector a
cross (GLVector x1 y1 z1) (GLVector x2 y2 z2) =
  GLVector (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)
