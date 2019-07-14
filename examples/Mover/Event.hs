module Event where

import Data.HashMap.Strict as HM
import Data.Maybe

import Debug.Trace

import Clock
import Config
import ExData
import Logic

setTheHoliday :: Timeline -> Record -> Maybe Record
setTheHoliday _ aRecord =
  if (isFebruary && isFirstMonday)
    then Just (HM.insert keyIsHoliday (BV True) aRecord)
    else Nothing
  where
    theMonth = takeIV $ aRecord HM.! keyMonth
    isFebruary = theMonth == 2
    theDay = takeIV $ aRecord HM.! keyDay
    theDoW = toEnum' $ aRecord HM.! keyDoW
    isFirstMonday = theDay < 7 && theDoW == Mon
