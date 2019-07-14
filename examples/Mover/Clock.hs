-- Functions for Date and Time with clock

module Clock where

import Data.Vector ((!))

import Data.Vector (Vector)
import qualified Data.Vector as V

type Date = (Int,Int,Int)
data DoW = Sun | Mon | Tus | Wed | Thr | Fri | Sat deriving (Show,Eq,Ord,Enum,Bounded)

baseMonthDays, baseLMonthDays :: Vector Int
baseMonthDays  = V.fromList [0,31,28,31,30,31,30,31,31,30,31,30,31]
baseLMonthDays = V.fromList [0,31,29,31,30,31,30,31,31,30,31,30,31]

baseYear :: Int
baseYear = 1980

type Day = DoW

data Config
  = Config
    { startOfADay     :: {-# UNPACK #-} !Int
    , hoursOfADay     :: {-# UNPACK #-} !Int
    , minutesOfAHour  :: {-# UNPACK #-} !Int
    , minuteTimeUnit  :: {-# UNPACK #-} !Int
    , slotsOfAHour    :: {-# UNPACK #-} !Int
    , endClock        :: {-# UNPACK #-} !Int
    }


zeller :: Int -> Int -> Int -> Int
zeller year month day = mod (day + div (13 * m - 1) 5 + d + div d 4 + div c 4 - 2 * c) 7
  where
    y = if month < 3 then year - 1 else year
    m = if month < 3 then month + 10 else month - 2
    (c, d) = divMod y 100

getDay :: Int -> Int -> Int -> Day
getDay y m d = toEnum $ zeller y m d

getNextDay :: Day -> Day
getNextDay dy = toEnum $ mod (fromEnum dy + 1) 7

getSerialDay :: Config -> Int -> Int
getSerialDay conf time = div time ((hoursOfADay conf) * (slotsOfAHour conf))

isLeapYear :: Int -> Bool
isLeapYear year =
  if (mod year 4) == 0
    then if (mod year 100) == 0
      then (mod year 400) == 0
      else True
    else False


getDate :: Config -> Int -> (Int,Int,Int,Day)
getDate conf time =
  let serialDay = getSerialDay conf time
  in  getYear baseYear serialDay
  where
    getYear :: Int -> Int -> (Int,Int,Int,Day)
    getYear year day =
      let baseDays = if isLeapYear year then 366 else 365
      in if day >= baseDays
        then getYear (year+1) (day - baseDays)
        else getMonth year 1 day
    getMonth :: Int -> Int -> Int -> (Int,Int,Int,Day)
    getMonth year month day =
      let baseDays = if isLeapYear year
          then baseLMonthDays ! month
          else baseMonthDays ! month
      in  if day >= baseDays
          then getMonth year (month+1) (day - baseDays)
          else (year,month,day+1,getDay year month (day+1))

getTime :: Config -> Int -> Int -> Int -> Int
getTime conf y m d
  | y < baseYear = error "getTime: Before baseYear"
  | otherwise = getTimeByYear baseYear 0
  where
    getTimeByYear :: Int -> Int -> Int
    getTimeByYear yc time
      | y < yc  = error "getTimeByYear: Overflooding"
      | y == yc = getTimeByMonth 1 time
      | otherwise = let baseDays = if isLeapYear yc then 366 else 365
                    in getTimeByYear (yc+1) (time + baseDays * (hoursOfADay conf) * (slotsOfAHour conf))
    getTimeByMonth :: Int -> Int -> Int
    getTimeByMonth mc time
      | m < 1 = error "getTimeByMonth: Non-positive month"
      | m < mc = error "getTimeByMonth: Overflooding"
      | mc == m = getTimeByDays time
      | otherwise = let baseDays = if isLeapYear y
                                    then baseLMonthDays ! mc
                                    else baseMonthDays ! mc
                    in  getTimeByMonth (mc+1) (time + baseDays * (hoursOfADay conf) * (slotsOfAHour conf))
    getTimeByDays :: Int -> Int
    getTimeByDays time = time + (d-1) * (hoursOfADay conf) * (slotsOfAHour conf)

getNextDateByDay :: Config -> Int -> Day -> (Int,Int,Int,Day)
getNextDateByDay conf time day = 
  let (y,m,d,dy) = getDate conf time
      dyi = fromEnum dy
      dayi = fromEnum day
  in  if dyi > dayi
      then getNextDateWithDays y m d (dayi + 7 - dyi)
      else getNextDateWithDays y m d (dayi - dyi)

-- FIXME: Reliable? but slow!
getNextDateWithDays' :: Config -> Int -> Int -> Int -> Int -> (Int,Int,Int,Day)
getNextDateWithDays' conf y m d oDays = getDate conf ( getTime conf y m d + oDays * (hoursOfADay conf) * (slotsOfAHour conf))

-- FIXME: Faster?
getNextDateWithDays  :: Int -> Int -> Int -> Int -> (Int,Int,Int,Day)
getNextDateWithDays  y m d oDays
  | oDays < 0 = error "Fail"
  | isLeapYear y =
      if d + oDays > baseLMonthDays ! m
        then if m == 12
                then getNextDateWithDays (y+1) 1 1 (d + oDays - baseLMonthDays ! m - 1)
                else getNextDateWithDays y (m+1) 1 (d + oDays - baseLMonthDays ! m - 1)
        else (y, m, d + oDays, getDay y m (d + oDays))
  | otherwise    =
      if d + oDays > baseMonthDays ! m
        then if m == 12
                then getNextDateWithDays (y+1) 1 1 (d + oDays - baseMonthDays ! m - 1)
                else getNextDateWithDays y (m+1) 1 (d + oDays - baseMonthDays ! m - 1)
        else (y, m, d + oDays, getDay y m (d + oDays))

{-
getClockString :: Config -> Int -> String
getClockString conf time =
    "Time(" ++ show time ++ "): " ++ hourStr ++ ":" ++ minuteStr
    where rHourStr = show $ getHourFromTime conf time
          hourStr = if isLength rHourStr 1
                        then '0':rHourStr
                        else rHourStr
          rMinuteStr = show $ getMinuteFromTime conf time
          minuteStr = if isLength rMinuteStr 1
                        then '0':rMinuteStr
                        else rMinuteStr

getTimeString :: Config -> Int -> String
getTimeString conf time =
    hourStr ++ ":" ++ minuteStr
    where rHourStr = show $ getHourFromTime conf time
          hourStr = if isLength rHourStr 1
                        then '0':rHourStr
                        else rHourStr
          rMinuteStr = show $ getMinuteFromTime conf time
          minuteStr = if isLength rMinuteStr 1
                        then '0':rMinuteStr
                        else rMinuteStr

getDateTimeString :: Config -> Int -> String
getDateTimeString conf time =
    show y ++ "/" ++ show m ++ "/" ++ show d ++ "/" ++ show dy ++ " " ++ hourStr ++ ":" ++ minuteStr
    where (y,m,d,dy) = getDate conf time
          rHourStr = show $ getHourFromTime conf time
          hourStr = if isLength rHourStr 1
                        then '0':rHourStr
                        else rHourStr
          rMinuteStr = show $ getMinuteFromTime conf time
          minuteStr = if isLength rMinuteStr 1
                        then '0':rMinuteStr
                        else rMinuteStr
-}
getMinuteFromTime :: Config -> Int -> Int
getMinuteFromTime conf time =
  let minuteTime = mod time (slotsOfAHour conf)
      minute = minuteTime * (minuteTimeUnit conf)
  in minute

getHourFromTime :: Config -> Int -> Int
getHourFromTime conf time =
  let hourTime = mod time ((hoursOfADay conf) * (slotsOfAHour conf))
      hour = div hourTime (slotsOfAHour conf)
  in hour

getHMFromTime :: Config -> Int -> (Int,Int)
getHMFromTime conf time =
  let baseTime = mod time ((hoursOfADay conf) * (slotsOfAHour conf))
      (hourUnit,minuteUnit) = divMod baseTime (slotsOfAHour conf)
      (hour,minute) = (hourUnit, minuteUnit * (minuteTimeUnit conf))
  in (hour,minute)
