{-# LANGUAGE OverloadedStrings #-}

module Config where

import Clock

import Data.Text

defConfig = Config
  { startOfADay = 0
  , hoursOfADay = 24
  , minutesOfAHour = 60
  , minuteTimeUnit = 1
  , slotsOfAHour = 1
  , endClock = 0
  }

keyYear, keyMonth, keyDay, keyDoW, keyIsHoliday :: Text
keyYear = "year"
keyMonth = "month"
keyDay = "day"
keyDoW = "dow"
keyIsHoliday = "isHoliday"
