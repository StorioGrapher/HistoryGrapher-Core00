module Main where

import Control.Monad

import Data.Foldable
import Data.IntMap as IM
import Data.HashMap.Strict as HM
import Data.Maybe

--import Data


import Event
import ExData
import Clock
import Config
import Logic

-- No dynamic column
-- Date data is prepared


blankData = generateTimeline 500


main :: IO ()
main = do
  putStrLn "Start HistoryGrapher-Core00 Mover examples"
  putStrLn "  Purpose 01: Move event(spool) mover with the anchor system"
  putStrLn "    Scenario 01-01: Allocate Holiday which related with days of week or days of month"
  printerWith (\x -> length x /= 4) . interpreter [setTheHoliday] $ blankData
  putStrLn "  Purpose 02: Evaluate values based on event"
  putStrLn "    Scenario 02-01: Evaluate monthly salary and bonus based on characters and events on month"

shower aRecord = yStr ++ mStr ++ dStr ++ " " ++ dowStr ++ " " ++ holidayStr
  where
    yStr = show . takeIV $ aRecord HM.! keyYear
    rawMStr = show . takeIV $ aRecord HM.! keyMonth
    mStr = replicate (3 - length rawMStr) ' ' ++ rawMStr
    rawDStr = show . takeIV $ aRecord HM.! keyDay
    dStr = replicate (3 - length rawDStr) ' ' ++ rawDStr
    dowStr = show . toEnum' $ aRecord HM.! keyDoW
    holidayStr = maybe "" (show . takeBV) (HM.lookup keyIsHoliday aRecord)

--printer = mapM_ (putStrLn . shower)
printerWith condF = mapM_ (\x -> when (condF x) (putStrLn . shower $ x))
