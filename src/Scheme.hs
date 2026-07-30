module Scheme
  ( Env,
    EvalError,
    Executable,
    InterpreterError (..),
    ParseError (..),
    SExpr (..),
    Value,
    analyze,
    eval,
    execute,
    newGlobalEnv,
    readExpr,
    readProgram,
    renderEvalError,
    renderInterpreterError,
    renderParseError,
    renderValue,
    runSource,
  )
where

import Scheme.Eval
  ( Env,
    EvalError,
    Executable,
    Value,
    analyze,
    eval,
    execute,
    newGlobalEnv,
    renderEvalError,
    renderValue,
  )
import Scheme.Parser (ParseError (..), parseExpr, parseProgram, renderParseError)
import Scheme.Syntax (SExpr (..))

data InterpreterError
  = ReaderError ParseError
  | EvaluationError EvalError

instance Show InterpreterError where
  show = renderInterpreterError

readExpr :: String -> Either ParseError SExpr
readExpr = parseExpr

readProgram :: String -> Either ParseError [SExpr]
readProgram = parseProgram

runSource :: Env -> String -> IO (Either InterpreterError [Value])
runSource env source = case readProgram source of
  Left err -> pure (Left (ReaderError err))
  Right expressions -> evaluateAll [] expressions
  where
    evaluateAll reversed [] = pure (Right (reverse reversed))
    evaluateAll reversed (expression : rest) = do
      result <- eval env expression
      case result of
        Left err -> pure (Left (EvaluationError err))
        Right value -> evaluateAll (value : reversed) rest

renderInterpreterError :: InterpreterError -> String
renderInterpreterError err = case err of
  ReaderError parseError -> "read error: " ++ renderParseError parseError
  EvaluationError evalError -> "evaluation error: " ++ renderEvalError evalError
