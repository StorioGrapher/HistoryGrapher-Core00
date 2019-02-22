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
type TimeSpan = Maybe (Time,Time)

-- No Branch World yet
data WorldState = WorldState
  { valueTable   :: ValueTable
  , controlTable :: ControlTable
  , evaluatedSpan  :: TimeSpan
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

-- # Condition
-- * Edit only for current data

type LocalID = ID

data CERES
  = InitValue ID Value
  | SetValue  ID Value
  | DeleteValue ID
  | ModifyValue ID CERESArithmeticOperator
  | CopyValue ID ID
  | CopyLocal ID LocalID

data CERESArithmeticOperator
  = CAOMul Int
  | CAOAdd Int
  | CAOSub Int
  | CAODiv Int
  | CAOMod Int
