module Logic where

import Data.IntMap as IM
import Data.HashMap.Strict as HM
import Data.Maybe

import Debug.Trace

import Clock
import Config
import ExData

generateTimeline  :: Int -> Timeline
generateTimeline limit = generateTimelineSub 0 IM.empty
  where
    generateTimelineSub :: Int -> Timeline -> Timeline
    generateTimelineSub n timeline =
      if n == limit
        then timeline
        else generateTimelineSub (n+1) $ IM.insert n (generateRecord n) timeline

generateRecord n = HM.fromList $
    [ (keyYear, IV theYear)
    , (keyMonth, IV theMonth)
    , (keyDay, IV theDay)
    , (keyDoW, IV (fromEnum theDoW))
    ]
  where
    (theYear,theMonth,theDay,theDoW) = getDate defConfig (n*24)

interpreter events timeline = interpreterSub timeline 0
  where
    end = IM.size timeline
    interpreterSub timeline t =
      if t == end
      then timeline
      else tryEvents timeline t events
    tryEvents timeline t [] = interpreterSub timeline (t+1)
    tryEvents timeline t (e:es) = tryEvents (maybe timeline (\r -> IM.insert t r timeline) result) t es
      where
        result = e timeline (timeline IM.! t)

toEnum' :: Value -> DoW
toEnum' (IV v) = toEnum v
toEnum' v = error $ "[ERROR]<toEnum'>: Can't handle value like " ++ show v

takeIV :: Value -> Int
takeIV (IV v) = v
takeIV v = error $ "[ERROR]<takeIV>: Can't take Int value from " ++ show v
