module Main where

import Data.IntMap
import Data.Maybe

import Debug.Trace

import Data
import Data.Initialize

initializeIntValueAt0 :: World -> World
initializeIntValueAt0 (World cs vl ws) = World cs newVL newWS
  where
    time = 0
    initialValue = 0
    -- Step: Add ValueTyper to ValueList
    (vtID,newVL) = addValueTyper vl (ValueTyper "Int" VTInt)
    -- Step: Add Value to specified ValueRow
    newWS = initializeValueAt ws time (IntValue initialValue) vtID

addValueTyper vl vt = (newID,newVL)
  where
    newID = 1 + maybe 0 fst (lookupMax vl)
    newVL = insert newID vt vl

initializeValueAt ws time value valueID = ws {evaluatedSpan = newSpan, valueTable = newValueTable}
  where
    aValueTable = valueTable ws
    headTime = fst . fromJust . evaluatedSpan $ ws
    endTime = snd . fromJust . evaluatedSpan $ ws
    singleRow = ValueRow time (fromList [(valueID,value)])
    isValueTableEmpty = Data.IntMap.null aValueTable
    beforeValueTable = if isJust (evaluatedSpan ws)
      then if headTime <= time
        then aValueTable
        else Prelude.foldr (\t -> insert t (singleRow{valueRowTime = t}) ) aValueTable [time .. headTime - 1]
      else aValueTable
    newPreValueTable = if isValueTableEmpty
      then fromList [(time,singleRow)]
      else mapWithKey (conditionalInitializeValue valueID value) aValueTable
    conditionalInitializeValue valueID value rowTime (ValueRow vrt values) =
      if rowTime < time
        then ValueRow vrt values
        else ValueRow vrt newValues
      where
        newValues = insert valueID value values
    newValueTable = if isJust (evaluatedSpan ws) && (endTime < time)
      then
        Prelude.foldr (\t -> insert t (lastRow {valueRowTime = t})) newPreValueTable [endTime+1 .. time]
      else newPreValueTable
      where
        lastRow = snd . findMax $ newPreValueTable
    newSpan = if isValueTableEmpty then Just (time,time) else Just (headTime, newEndTime)
      where
        newEndTime = if endTime < time then time else endTime

main = do
  putStrLn "Start HistoryGrapher-Core Interpreter"
  putStrLn "Start with TestData"
  -- Prepare World
  print emptyWorld
  -- Add an Control - Value Initializer
  let newWorld = initializeIntValueAt0 $ initializeIntValueAt0 emptyWorld

  print newWorld
