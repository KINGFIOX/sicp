module Scheme.Primitives (primitiveBindings) where

import Control.Monad.Trans.Except (throwE)
import Control.Monad.IO.Class (liftIO)
import Scheme.Runtime
  ( Eval,
    EvalError (..),
    Value (..),
    isTrue,
    renderValue,
  )
import Scheme.Syntax (Name)

primitiveBindings :: [(Name, Value)]
primitiveBindings =
  map makePrimitive
    [ ("+", integerFold "+" 0 (+)),
      ("*", integerFold "*" 1 (*)),
      ("-", subtractPrimitive),
      ("/", dividePrimitive),
      ("=", integerComparison "=" (==)),
      ("<", integerComparison "<" (<)),
      ("<=", integerComparison "<=" (<=)),
      (">", integerComparison ">" (>)),
      (">=", integerComparison ">=" (>=)),
      ("cons", binary "cons" (\first rest -> pure (VPair first rest))),
      ("car", unary "car" carPrimitive),
      ("cdr", unary "cdr" cdrPrimitive),
      ("list", pure . foldr VPair VNil),
      ("null?", unary "null?" (pure . VBoolean . isNull)),
      ("pair?", unary "pair?" (pure . VBoolean . isPair)),
      ("eq?", binary "eq?" (\left right -> pure (VBoolean (eqValue left right)))),
      ("equal?", binary "equal?" (\left right -> pure (VBoolean (equalValue left right)))),
      ("not", unary "not" (pure . VBoolean . not . isTrue)),
      ("number?", unary "number?" (pure . VBoolean . isNumber)),
      ("symbol?", unary "symbol?" (pure . VBoolean . isSymbol)),
      ("string?", unary "string?" (pure . VBoolean . isString)),
      ("boolean?", unary "boolean?" (pure . VBoolean . isBoolean)),
      ("procedure?", unary "procedure?" (pure . VBoolean . isProcedure)),
      ("display", unary "display" displayPrimitive),
      ("newline", nullary "newline" newlinePrimitive)
    ]
  where
    makePrimitive (name, function) = (name, VPrimitive name function)

integerFold :: Name -> Integer -> (Integer -> Integer -> Integer) -> [Value] -> Eval Value
integerFold name identity operation values =
  VInteger . foldl operation identity <$> traverse (expectInteger name) values

subtractPrimitive :: [Value] -> Eval Value
subtractPrimitive [] = throwE (WrongArity "-" "at least 1" 0)
subtractPrimitive values = do
  integers <- traverse (expectInteger "-") values
  pure $ VInteger $ case integers of
    [value] -> negate value
    first : rest -> foldl (-) first rest
    [] -> 0

dividePrimitive :: [Value] -> Eval Value
dividePrimitive [] = throwE (WrongArity "/" "at least 1" 0)
dividePrimitive values = do
  integers <- traverse (expectInteger "/") values
  case integers of
    [value] -> VInteger <$> checkedQuot 1 value
    first : rest -> VInteger <$> foldChecked first rest
    [] -> pure (VInteger 1)
  where
    foldChecked accumulator [] = pure accumulator
    foldChecked accumulator (value : rest) = checkedQuot accumulator value >>= (`foldChecked` rest)

checkedQuot :: Integer -> Integer -> Eval Integer
checkedQuot _ 0 = throwE DivisionByZero
checkedQuot numerator denominator = pure (quot numerator denominator)

integerComparison :: Name -> (Integer -> Integer -> Bool) -> [Value] -> Eval Value
integerComparison name relation values
  | length values < 2 = throwE (WrongArity name "at least 2" (length values))
  | otherwise = do
      integers <- traverse (expectInteger name) values
      pure (VBoolean (and (zipWith relation integers (drop 1 integers))))

expectInteger :: Name -> Value -> Eval Integer
expectInteger _ (VInteger value) = pure value
expectInteger name value = throwE (WrongType name "an integer" value)

unary :: Name -> (Value -> Eval Value) -> [Value] -> Eval Value
unary _ function [value] = function value
unary name _ values = throwE (WrongArity name "1" (length values))

binary :: Name -> (Value -> Value -> Eval Value) -> [Value] -> Eval Value
binary _ function [left, right] = function left right
binary name _ values = throwE (WrongArity name "2" (length values))

nullary :: Name -> Eval Value -> [Value] -> Eval Value
nullary _ action [] = action
nullary name _ values = throwE (WrongArity name "0" (length values))

carPrimitive :: Value -> Eval Value
carPrimitive (VPair first _) = pure first
carPrimitive value = throwE (WrongType "car" "a pair" value)

cdrPrimitive :: Value -> Eval Value
cdrPrimitive (VPair _ rest) = pure rest
cdrPrimitive value = throwE (WrongType "cdr" "a pair" value)

displayPrimitive :: Value -> Eval Value
displayPrimitive (VString value) = liftIO (putStr value) >> pure ok
displayPrimitive value = liftIO (putStr (renderValue value)) >> pure ok

newlinePrimitive :: Eval Value
newlinePrimitive = liftIO (putStrLn "") >> pure ok

ok :: Value
ok = VSymbol "ok"

isNull, isPair, isNumber, isSymbol, isString, isBoolean, isProcedure :: Value -> Bool
isNull VNil = True
isNull _ = False
isPair VPair {} = True
isPair _ = False
isNumber VInteger {} = True
isNumber _ = False
isSymbol VSymbol {} = True
isSymbol _ = False
isString VString {} = True
isString _ = False
isBoolean VBoolean {} = True
isBoolean _ = False
isProcedure VPrimitive {} = True
isProcedure VClosure {} = True
isProcedure _ = False

eqValue :: Value -> Value -> Bool
eqValue (VInteger left) (VInteger right) = left == right
eqValue (VString left) (VString right) = left == right
eqValue (VBoolean left) (VBoolean right) = left == right
eqValue (VSymbol left) (VSymbol right) = left == right
eqValue VNil VNil = True
eqValue _ _ = False

equalValue :: Value -> Value -> Bool
equalValue (VPair leftFirst leftRest) (VPair rightFirst rightRest) =
  equalValue leftFirst rightFirst && equalValue leftRest rightRest
equalValue left right = eqValue left right
