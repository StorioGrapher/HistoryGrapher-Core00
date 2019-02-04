module Test.Data where

import Test.Framework
import Test.Framework.Providers.HUnit
--import qualified Test.Framework.Providers.QuickCheck2 as Test
import Test.Framework.TH
import Test.HUnit.Base

import Data

tests = $(testGroupGenerator)

i1 = IntValue 1
i2 = IntValue 2
s1 = StrValue "S"

case_eq01 = i1 ~?= i1
case_eq02 = assertBool "i1 /= i2" (i1 /= i2)
case_eq03 = assertBool "i1 /= s1" (i1 /= s1)
