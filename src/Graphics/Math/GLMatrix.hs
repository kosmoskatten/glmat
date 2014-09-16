module Graphics.Math.GLMatrix
    ( Matrix4 (..)
    , identity4
    ) where

-- | A four by four matrix adapted for use with OpenGL. Implemented to
-- work in column major order.
data Matrix4 a = Matrix4 !a !a !a !a
                         !a !a !a !a
                         !a !a !a !a
                         !a !a !a !a
  deriving (Eq, Ord, Show)

-- | Construct the identity matrix for 4x4.
identity4 :: Num a => Matrix4 a
identity4 = Matrix4 1 0 0 0
                    0 1 0 0
                    0 0 1 0
                    0 0 0 1
