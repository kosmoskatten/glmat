module Main (main) where

import Test.Framework (Test, defaultMain)
import qualified UnitTests as UnitTests

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = UnitTests.suite
