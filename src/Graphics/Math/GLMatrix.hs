module Graphics.Math.GLMatrix
    ( GLMatrix (..)
    , identity
    , translate
    , scale
    , perspective
    , lookAt
    , transpose
    , (>*<)
    ) where

import Graphics.Math.GLVector (GLVector (..), normalize, cross, sub)

-- | A four by four matrix adapted for use with OpenGL. Implemented to
-- work in row major order.
data GLMatrix a = GLMatrix !a !a !a !a
                           !a !a !a !a
                           !a !a !a !a
                           !a !a !a !a
  deriving (Eq, Show)

-- | Construct the identity matrix.
identity :: Num a => GLMatrix a
identity = GLMatrix 1 0 0 0
                    0 1 0 0
                    0 0 1 0
                    0 0 0 1

-- | Construct a translation matrix.
translate :: Num a => a -> a -> a -> GLMatrix a
translate x y z = GLMatrix 1 0 0 0
                           0 1 0 0
                           0 0 1 0
                           x y z 1

-- | Construct a scale matrix.
scale :: Num a => a -> a -> a -> GLMatrix a
scale x y z = GLMatrix x 0 0 0
                       0 y 0 0
                       0 0 z 0
                       0 0 0 1

-- | Construct the perspective matrix.
perspective :: Floating a => a -> a -> a -> a -> GLMatrix a
perspective fovy aspect zNear zFar =
  let fovy' = degToRad fovy
      f     = cot (fovy' / 2)
      
      m11   = f / aspect
      m12   = 0
      m13   = 0
      m14   = 0

      m21   = 0
      m22   = f
      m23   = 0
      m24   = 0

      m31   = 0
      m32   = 0
      m33   = (zFar + zNear) / (zNear - zFar)
      m34   = (-1)

      m41   = 0
      m42   = 0
      m43   = (2 * zFar * zNear) / (zNear - zFar)
      m44   = 0
  in GLMatrix m11 m12 m13 m14
              m21 m22 m23 m24
              m31 m32 m33 m34
              m41 m42 m43 m44

-- | Construct a lookAt matrix.
lookAt :: Floating a => GLVector a -> GLVector a -> GLVector a -> GLMatrix a
lookAt eye@(GLVector eyex eyey eyez) center up =
  let forward@(GLVector fx fy fz) = normalize $ center `sub` eye
      side@(GLVector sx sy sz)    = normalize $ forward `cross` up
      GLVector ux uy uz           = side `cross` forward
      matrix                      =
        GLMatrix sx ux (-fx) 0
                 sy uy (-fy) 0
                 sz uz (-fz) 0
                 0  0  0     1
  in translate (-eyex) (-eyey) (-eyez) >*< matrix

-- | Transpose a matrix.
transpose :: GLMatrix a -> GLMatrix a
transpose (GLMatrix m11 m12 m13 m14
                    m21 m22 m23 m24
                    m31 m32 m33 m34
                    m41 m42 m43 m44) =
  GLMatrix m11 m21 m31 m41
           m12 m22 m32 m42
           m13 m23 m33 m43
           m14 m24 m34 m44

-- | Multiply two matrices.
(>*<) :: Num a => GLMatrix a -> GLMatrix a -> GLMatrix a
(>*<) (GLMatrix a11 a12 a13 a14
                a21 a22 a23 a24
                a31 a32 a33 a34
                a41 a42 a43 a44)
      (GLMatrix b11 b12 b13 b14
                b21 b22 b23 b24
                b31 b32 b33 b34
                b41 b42 b43 b44) =
  let c11 = a11 * b11 + a12 * b21 + a13 * b31 + a14 * b41
      c12 = a11 * b12 + a12 * b22 + a13 * b32 + a14 * b42
      c13 = a11 * b13 + a12 * b23 + a13 * b33 + a14 * b43
      c14 = a11 * b14 + a12 * b24 + a13 * b34 + a14 * b44

      c21 = a21 * b11 + a22 * b21 + a23 * b31 + a24 * b41
      c22 = a21 * b12 + a22 * b22 + a23 * b32 + a24 * b42
      c23 = a21 * b13 + a22 * b23 + a23 * b33 + a24 * b43
      c24 = a21 * b14 + a22 * b24 + a23 * b34 + a24 * b44

      c31 = a31 * b11 + a32 * b21 + a33 * b31 + a34 * b41
      c32 = a31 * b12 + a32 * b22 + a33 * b32 + a34 * b42
      c33 = a31 * b13 + a32 * b23 + a33 * b33 + a34 * b43
      c34 = a31 * b14 + a32 * b24 + a33 * b34 + a34 * b44

      c41 = a41 * b11 + a42 * b21 + a43 * b31 + a44 * b41
      c42 = a41 * b12 + a42 * b22 + a43 * b32 + a44 * b42
      c43 = a41 * b13 + a42 * b23 + a43 * b33 + a44 * b43
      c44 = a41 * b14 + a42 * b24 + a43 * b34 + a44 * b44
  in GLMatrix c11 c12 c13 c14
              c21 c22 c23 c24
              c31 c32 c33 c34
              c41 c42 c43 c44

degToRad :: Floating a => a -> a
degToRad theta = theta * (pi / 180)

cot :: Floating a => a -> a
cot theta = 1 / (tan theta)

