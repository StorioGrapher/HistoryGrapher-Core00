module Data where

import Data.IntMap as IM

type Name = String
type Time = Int
type ID = Int

data World = World
  { controller :: Controller
  , valueList  :: ValueList
  , state      :: WorldState
  } deriving Show

type Controllers = IntMap Control

data Controller = Controller
  { controlMap   :: Controllers
  } deriving Show

type ValueList = IntMap ValueTyper

data ValueTyper = ValueTyper
  { valueTyperName :: Name
  , valueType :: ValueType
  } deriving (Show, Eq)

type ValueTable = IntMap ValueRow
type ControlTable = IntMap ControlRow

-- No Branch World yet
data WorldState = WorldState
  { valueTable   :: ValueTable
  , controlTable :: ControlTable
  } deriving Show

type Values = IntMap Value

data ValueRow = ValueRow
  { valueRowTime :: Time
  , values       :: Values
  } deriving Show

type Controls = IntMap Control

data ControlRow = ControlRow
  { controlRowTime :: Time
  , controls       :: Controls
  } deriving Show

data Control
  = Control
    { controlName :: Name
    , controlF    :: World -> World
    }

instance Show Control where
  show (Control name _)     = "Ctrl: " ++ name

data Value
  = IntValue { iV :: Int }
  | StrValue { sV :: String }
  deriving (Show, Eq)

data ValueType
  = VTInt
  | VTStr
  deriving (Show, Eq, Ord, Enum)
