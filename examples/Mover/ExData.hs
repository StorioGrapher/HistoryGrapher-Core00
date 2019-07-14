-- ExData which is independent from CEREScript

module ExData where

import Data.IntMap as IM
import Data.HashMap.Strict as HM
import Data.Text as T

import Clock

data Value = BV Bool | IV Int | DV Double | SV String deriving Show

type Record = HM.HashMap Text Value
type Timeline = IM.IntMap Record
