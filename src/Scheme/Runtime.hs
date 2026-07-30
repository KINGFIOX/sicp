module Scheme.Runtime
  ( Env,
    Eval,
    EvalError (..),
    Executable (..),
    Value (..),
    defineVariable,
    extendEnv,
    isTrue,
    lookupVariable,
    newRootEnv,
    quoteValue,
    renderEvalError,
    renderValue,
    setVariable,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (intercalate)
import Scheme.Syntax (Name, SExpr (..))

type Eval a = ExceptT EvalError IO a

newtype Executable = Executable {runExecutable :: Env -> Eval Value}

data Value
  = VInteger Integer
  | VString String
  | VBoolean Bool
  | VSymbol Name
  | VNil
  | VPair Value Value
  | VPrimitive Name ([Value] -> Eval Value)
  | VClosure [Name] Executable Env
  | VUnassigned

data EvalError
  = InvalidForm String SExpr
  | UnboundVariable Name
  | UnassignedVariable Name
  | WrongArity Name String Int
  | WrongType Name String Value
  | NotProcedure Value
  | DivisionByZero

instance Show EvalError where
  show = renderEvalError

type Cell = IORef Value

type Frame = IORef [(Name, Cell)]

data Env = Env Frame (Maybe Env)

newRootEnv :: [(Name, Value)] -> IO Env
newRootEnv = (`extendEnv` Nothing)

extendEnv :: [(Name, Value)] -> Maybe Env -> IO Env
extendEnv bindings parent = do
  cells <- traverse makeCell bindings
  frame <- newIORef cells
  pure (Env frame parent)
  where
    makeCell (name, value) = do
      cell <- newIORef value
      pure (name, cell)

lookupVariable :: Env -> Name -> Eval Value
lookupVariable env name = do
  found <- liftIO (findCell env name)
  case found of
    Nothing -> throwE (UnboundVariable name)
    Just cell -> do
      value <- liftIO (readIORef cell)
      case value of
        VUnassigned -> throwE (UnassignedVariable name)
        _ -> pure value

defineVariable :: Env -> Name -> Value -> Eval ()
defineVariable (Env frame _) name value = do
  bindings <- liftIO (readIORef frame)
  case lookup name bindings of
    Just cell -> liftIO (writeIORef cell value)
    Nothing -> do
      cell <- liftIO (newIORef value)
      liftIO (writeIORef frame ((name, cell) : bindings))

setVariable :: Env -> Name -> Value -> Eval ()
setVariable env name value = do
  found <- liftIO (findCell env name)
  case found of
    Nothing -> throwE (UnboundVariable name)
    Just cell -> liftIO (writeIORef cell value)

findCell :: Env -> Name -> IO (Maybe Cell)
findCell (Env frame parent) name = do
  bindings <- readIORef frame
  case lookup name bindings of
    Just cell -> pure (Just cell)
    Nothing -> maybe (pure Nothing) (`findCell` name) parent

isTrue :: Value -> Bool
isTrue (VBoolean False) = False
isTrue _ = True

quoteValue :: SExpr -> Value
quoteValue expression = case expression of
  Number value -> VInteger value
  StringLiteral value -> VString value
  Boolean value -> VBoolean value
  Symbol value -> VSymbol value
  List values -> foldr VPair VNil (map quoteValue values)
  DottedList values tailValue -> foldr VPair (quoteValue tailValue) (map quoteValue values)

renderValue :: Value -> String
renderValue value = case value of
  VInteger number -> show number
  VString text -> show text
  VBoolean True -> "#t"
  VBoolean False -> "#f"
  VSymbol name -> name
  VNil -> "()"
  VPair first rest -> "(" ++ renderValue first ++ renderPairTail rest
  VPrimitive name _ -> "<primitive:" ++ name ++ ">"
  VClosure {} -> "<procedure>"
  VUnassigned -> "<unassigned>"

renderPairTail :: Value -> String
renderPairTail value = case value of
  VNil -> ")"
  VPair first rest -> " " ++ renderValue first ++ renderPairTail rest
  other -> " . " ++ renderValue other ++ ")"

renderEvalError :: EvalError -> String
renderEvalError err = case err of
  InvalidForm message expression -> message ++ ": " ++ renderSExpr expression
  UnboundVariable name -> "unbound variable: " ++ name
  UnassignedVariable name -> "variable used before initialization: " ++ name
  WrongArity name expected actual ->
    name ++ ": expected " ++ expected ++ " argument(s), got " ++ show actual
  WrongType name expected actual ->
    name ++ ": expected " ++ expected ++ ", got " ++ renderValue actual
  NotProcedure value -> "attempted to call a non-procedure: " ++ renderValue value
  DivisionByZero -> "division by zero"

renderSExpr :: SExpr -> String
renderSExpr expression = case expression of
  Number value -> show value
  StringLiteral value -> show value
  Boolean True -> "#t"
  Boolean False -> "#f"
  Symbol name -> name
  List values -> "(" ++ unwords (map renderSExpr values) ++ ")"
  DottedList values tailValue ->
    "(" ++ intercalate " " (map renderSExpr values) ++ " . " ++ renderSExpr tailValue ++ ")"
