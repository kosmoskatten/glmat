module Main (main) where

import Test.Framework (Test, defaultMain)
import qualified GLMatrixTests as GMT
import qualified GLVectorTests as GVT
import qualified GLMatrixProps as GMP
import qualified GLVectorProps as GVP

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = GMT.suite ++ GVT.suite ++ GMP.suite ++ GVP.suite
