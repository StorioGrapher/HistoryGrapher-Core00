import Data

import qualified System.IO.Silently as Silently

import qualified Test.Framework as Test
import qualified Test.Framework.Providers.HUnit as Test
import qualified Test.Framework.Providers.QuickCheck2 as Test
import Test.HUnit
import Test.QuickCheck

import Test.Data


main :: IO ()
main = do
  -- captured <- Silently.capture_ something
  Test.defaultMain [
    Test.Data.tests
    ]
