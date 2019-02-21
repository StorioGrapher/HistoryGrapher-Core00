module Data.Initialize where

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Data


emptyWorld
  = World
      emptyController
      emptyValueList
      emptyWorldState

emptyController :: Controller
emptyController = Controller IM.empty

emptyWorldState :: WorldState
emptyWorldState
  = WorldState
      emptyValueTable
      emptyControlTable
      Nothing

emptyValueTable :: ValueTable
emptyValueTable = IM.empty

emptyControlTable :: ControlTable
emptyControlTable = IM.empty

emptyValueList :: ValueList
emptyValueList = IM.empty

emptyValueRow :: ValueRow
emptyValueRow = ValueRow 0 IM.empty

emptyValueRowAt :: Time -> ValueRow
emptyValueRowAt time = ValueRow time IM.empty

emptyControlRow :: ControlRow
emptyControlRow = ControlRow 0 IM.empty

emptyControlRowAt :: Time -> ControlRow
emptyControlRowAt time = ControlRow time IM.empty
