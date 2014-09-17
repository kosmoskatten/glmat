module Graphics.Math.GLUniformMatrix
       ( uniformMatrixF
       ) where

import Foreign.Marshal.Array (withArray)
import Graphics.Math.GLMatrix (GLMatrix (..))
import Graphics.Rendering.OpenGL
  ( UniformLocation (..)
  , GLfloat
  )
import Graphics.Rendering.OpenGL.Raw
  ( gl_FALSE
  , glUniformMatrix4fv
  )

uniformMatrixF :: UniformLocation -> GLMatrix GLfloat -> IO ()
uniformMatrixF (UniformLocation loc)
               (GLMatrix m11 m12 m13 m14
                         m21 m22 m23 m24
                         m31 m32 m33 m34
                         m41 m42 m43 m44) = do
  let mat = [ m11, m12, m13, m14
            , m21, m22, m23, m24
            , m31, m32, m33, m34
            , m41, m42, m43, m44 ]
  withArray mat $ glUniformMatrix4fv loc 1 (fromIntegral gl_FALSE)
