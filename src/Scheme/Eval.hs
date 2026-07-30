module Scheme.Eval
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
where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.List (group, sort)
import Scheme.Primitives (primitiveBindings)
import Scheme.Runtime
import Scheme.Syntax (Name, SExpr (..))

analyze :: SExpr -> Either EvalError Executable
analyze expression = Executable <$> compile expression

execute :: Env -> Executable -> IO (Either EvalError Value)
execute env executable = runExceptT (runExecutable executable env)

eval :: Env -> SExpr -> IO (Either EvalError Value)
eval env expression = case analyze expression of
  Left err -> pure (Left err)
  Right executable -> execute env executable

newGlobalEnv :: IO Env
newGlobalEnv =
  newRootEnv
    ( [ ("true", VBoolean True),
        ("false", VBoolean False)
      ]
        ++ primitiveBindings
    )

type Execution = Env -> Eval Value

compile :: SExpr -> Either EvalError Execution
compile expression = case expression of
  Number value -> pure (const (pure (VInteger value)))
  StringLiteral value -> pure (const (pure (VString value)))
  Boolean value -> pure (const (pure (VBoolean value)))
  Symbol name -> pure (`lookupVariable` name)
  DottedList {} -> invalid "a dotted list cannot be evaluated as an expression" expression
  List [] -> invalid "an empty list cannot be evaluated as an application" expression
  List (Symbol "quote" : arguments) -> compileQuote expression arguments
  List (Symbol "set!" : arguments) -> compileAssignment expression arguments
  List (Symbol "define" : arguments) -> compileDefinition expression arguments
  List (Symbol "if" : arguments) -> compileIf expression arguments
  List (Symbol "lambda" : arguments) -> compileLambda expression arguments
  List (Symbol "begin" : expressions) -> compileSequenceForm expression expressions
  List (Symbol "cond" : clauses) -> expandCond expression clauses >>= compile
  List (operator : operands) -> compileApplication operator operands

compileQuote :: SExpr -> [SExpr] -> Either EvalError Execution
compileQuote _ [value] = pure (const (pure (quoteValue value)))
compileQuote form _ = invalid "quote expects exactly one argument" form

compileAssignment :: SExpr -> [SExpr] -> Either EvalError Execution
compileAssignment _ [Symbol name, valueExpression] = do
  valueExecution <- compile valueExpression
  pure $ \env -> do
    value <- valueExecution env
    setVariable env name value
    pure ok
compileAssignment form _ = invalid "set! expects a variable and a value" form

compileDefinition :: SExpr -> [SExpr] -> Either EvalError Execution
compileDefinition form arguments = do
  (name, valueExpression) <- definitionParts form arguments
  valueExecution <- compile valueExpression
  pure $ \env -> do
    value <- valueExecution env
    defineVariable env name value
    pure ok

compileIf :: SExpr -> [SExpr] -> Either EvalError Execution
compileIf _ [predicate, consequent] = compileIfParts predicate consequent (Boolean False)
compileIf _ [predicate, consequent, alternative] = compileIfParts predicate consequent alternative
compileIf form _ = invalid "if expects two or three arguments" form

compileIfParts :: SExpr -> SExpr -> SExpr -> Either EvalError Execution
compileIfParts predicate consequent alternative = do
  predicateExecution <- compile predicate
  consequentExecution <- compile consequent
  alternativeExecution <- compile alternative
  pure $ \env -> do
    result <- predicateExecution env
    if isTrue result then consequentExecution env else alternativeExecution env

compileLambda :: SExpr -> [SExpr] -> Either EvalError Execution
compileLambda form (List parameters : body) = do
  names <- traverse parameterName parameters
  ensureDistinct "duplicate lambda parameter" form names
  whenEither (null body) (InvalidForm "lambda requires a body" form)
  bodyExecution <- compileProcedureBody form body
  pure $ \env -> pure (VClosure names (Executable bodyExecution) env)
compileLambda form _ = invalid "lambda expects a proper parameter list and a body" form

compileProcedureBody :: SExpr -> [SExpr] -> Either EvalError Execution
compileProcedureBody lambdaForm body = do
  let (definitionForms, expressions) = span isDefinition body
  whenEither (any isDefinition expressions) (InvalidForm "internal definitions must precede expressions" lambdaForm)
  whenEither (null expressions) (InvalidForm "a procedure body requires an expression after its definitions" lambdaForm)
  definitions <- traverse internalDefinition definitionForms
  ensureDistinct "duplicate internal definition" lambdaForm (map fst definitions)
  initializationExecutions <- traverse (compile . snd) definitions
  resultExecution <- compileSequence expressions
  pure $ \env -> do
    forM_ (map fst definitions) $ \name -> defineVariable env name VUnassigned
    sequence_
      [ initialization env >>= setVariable env name
        | ((name, _), initialization) <- zip definitions initializationExecutions
      ]
    resultExecution env

compileSequenceForm :: SExpr -> [SExpr] -> Either EvalError Execution
compileSequenceForm form [] = invalid "begin requires at least one expression" form
compileSequenceForm _ expressions = compileSequence expressions

compileSequence :: [SExpr] -> Either EvalError Execution
compileSequence expressions = do
  executions <- traverse compile expressions
  pure $ \env -> evaluateSequence env executions

evaluateSequence :: Env -> [Execution] -> Eval Value
evaluateSequence _ [] = pure ok
evaluateSequence env [execution] = execution env
evaluateSequence env (execution : rest) = execution env >> evaluateSequence env rest

compileApplication :: SExpr -> [SExpr] -> Either EvalError Execution
compileApplication operator operands = do
  operatorExecution <- compile operator
  operandExecutions <- traverse compile operands
  pure $ \env -> do
    procedure <- operatorExecution env
    arguments <- traverse ($ env) operandExecutions
    applyProcedure procedure arguments

applyProcedure :: Value -> [Value] -> Eval Value
applyProcedure (VPrimitive _ function) arguments = function arguments
applyProcedure (VClosure parameters body closureEnv) arguments
  | length parameters /= length arguments =
      throwE (WrongArity "procedure" (show (length parameters)) (length arguments))
  | otherwise = do
      callEnv <- liftIO (extendEnv (zip parameters arguments) (Just closureEnv))
      runExecutable body callEnv
applyProcedure value _ = throwE (NotProcedure value)

definitionParts :: SExpr -> [SExpr] -> Either EvalError (Name, SExpr)
definitionParts _ [Symbol name, value] = pure (name, value)
definitionParts _ (List (Symbol name : parameters) : body)
  | not (null body) = pure (name, List (Symbol "lambda" : List parameters : body))
definitionParts form _ = invalid "define expects a name and value, or a procedure header and body" form

internalDefinition :: SExpr -> Either EvalError (Name, SExpr)
internalDefinition form@(List (Symbol "define" : arguments)) = definitionParts form arguments
internalDefinition form = invalid "invalid internal definition" form

isDefinition :: SExpr -> Bool
isDefinition (List (Symbol "define" : _)) = True
isDefinition _ = False

expandCond :: SExpr -> [SExpr] -> Either EvalError SExpr
expandCond _ [] = pure (Boolean False)
expandCond whole (clause : rest) = case clause of
  List (Symbol "else" : actions)
    | null actions -> invalid "cond else clause requires an expression" clause
    | not (null rest) -> invalid "cond else clause must be last" whole
    | otherwise -> pure (sequenceExpression actions)
  List (predicate : actions)
    | null actions -> invalid "cond clause requires an expression" clause
    | otherwise -> do
        alternative <- expandCond whole rest
        pure (List [Symbol "if", predicate, sequenceExpression actions, alternative])
  _ -> invalid "cond clauses must be non-empty lists" clause

sequenceExpression :: [SExpr] -> SExpr
sequenceExpression [expression] = expression
sequenceExpression expressions = List (Symbol "begin" : expressions)

parameterName :: SExpr -> Either EvalError Name
parameterName (Symbol name) = pure name
parameterName expression = invalid "lambda parameters must be symbols" expression

ensureDistinct :: String -> SExpr -> [Name] -> Either EvalError ()
ensureDistinct message form names =
  whenEither (any ((> 1) . length) (group (sort names))) (InvalidForm message form)

whenEither :: Bool -> EvalError -> Either EvalError ()
whenEither condition err = if condition then Left err else Right ()

invalid :: String -> SExpr -> Either EvalError a
invalid message form = Left (InvalidForm message form)

ok :: Value
ok = VSymbol "ok"
