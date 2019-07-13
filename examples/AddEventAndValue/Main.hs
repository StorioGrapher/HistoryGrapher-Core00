module Main where

import Data.IntMap as IM
import Data.Maybe

import Debug.Trace

import Data

-- # Properties
-- * Do not manage ValueList

interpreter :: Env -> [CERES] -> Env
interpreter env [] = env
interpreter env (aCeres:ceres) =
  interpreter (interpreterSub env aCeres) ceres

interpreterSub env (InitValue   id value) = insert id value env
interpreterSub env (SetValue    id value) = insert id value env
interpreterSub env (DeleteValue id) = delete id env
interpreterSub env (ModifyValue id ao) = adjust modifier id env
  where
    modifier = case ao of
      (CAOMul o) -> (\(IntValue iv) -> IntValue (iv * o))
      (CAOAdd o) -> (\(IntValue iv) -> IntValue (iv + o))
      (CAOSub o) -> (\(IntValue iv) -> IntValue (iv - o))
      (CAODiv o) -> (\(IntValue iv) -> IntValue (div iv o))
      (CAOMod o) -> (\(IntValue iv) -> IntValue (mod iv o))
interpreterSub env (CopyValue from to) = insert to theValue env
  where
    theValue = fromJust $ IM.lookup from env
interpreterSub env (CopyLocal from to) = env

modifier01 (IntValue iv) = IntValue (iv * 2)
modifier01 (StrValue sv) = StrValue (sv ++ reverse sv)


-- ! Should omit !
-- * Change this as a Script form

type Env = IntMap Value
runCeres :: Env -> [CERES] -> IO Env
runCeres env [] = return env
runCeres env (aCeres:ceres) = do
  let nextEnv = interpreterSub env aCeres
  putStrLn "----------------"
  viewEnv nextEnv
  runCeres nextEnv ceres

viewEnv :: Env -> IO ()
viewEnv env = mapM_ (\x -> putStrLn $ "VE: " ++ show x) env

main = do
  putStrLn "CERES Interpreter Prototype"
  -- Prepare World
  let initialEnv = IM.empty
  viewEnv initialEnv

  let ceress =
        [ InitValue 0 (IntValue 2)
        , InitValue 1 (StrValue "Nothing")
        , SetValue  0 (IntValue 3)
        , SetValue  2 (StrValue "Something")
        , DeleteValue 1
        , ModifyValue 0 (CAOMul 2)
        ]
  runCeres initialEnv ceress
  putStrLn "End"
