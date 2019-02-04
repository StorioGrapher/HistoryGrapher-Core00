module Data where

import Data.IntMap as IM

type Name = String
type Time = Int
type ID = Int

data World = World
  { controller :: Controller
  , state      :: WorldState
  } deriving Show

type Controllers = IntMap Control

data Controller = Controller
  { controlMap   :: IntMap Control
  } deriving Show

type ValueTable = IntMap ValueRow
type ControlTable = IntMap ControlRow

-- No Branch World yet
data WorldState = WorldState
  { valueTable   :: IntMap ValueRow
  , controlTable :: IntMap ControlRow
  } deriving Show

data ValueRow = ValueRow
  { valueRowTime :: Time
  , values       :: IntMap Value
  } deriving Show

data ControlRow = ControlRow
  { controlRowTime :: Time
  , controls       :: IntMap Control
  } deriving Show

data Control = Control
  { controlName :: Name
  , controlID   :: ID
  , controlF    :: WorldState -> WorldState
  }

instance Show Control where
  show c = "Control(" ++ show (controlID c) ++ "): " ++ (controlName c)

data Value
  = IntValue { iV :: Int }
  | StrValue { sV :: String }
  deriving (Show, Eq)
