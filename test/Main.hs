module Main (main) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, displayException, try)
import Control.Monad (forM)
import Data.List (isInfixOf)
import Scheme
import Scheme.Parser (SourcePos (..), char, eof, runParser, string, tryP)
import System.Exit (exitFailure)

data Test = Test String (IO ())

main :: IO ()
main = do
  outcomes <- forM tests runTest
  let failures = length (filter not outcomes)
  if failures == 0
    then putStrLn (show (length tests) ++ " tests passed")
    else do
      putStrLn (show failures ++ " test(s) failed")
      exitFailure

runTest :: Test -> IO Bool
runTest (Test name action) = do
  result <- try action :: IO (Either SomeException ())
  case result of
    Right () -> putStrLn ("PASS " ++ name) >> pure True
    Left err -> putStrLn ("FAIL " ++ name ++ ": " ++ displayException err) >> pure False

tests :: [Test]
tests =
  [ Test "parser combinators support explicit backtracking" $ do
      let parser = tryP (string "ab") <|> string "ac"
      assertEqual "shared prefix" (Right "ac") (runParser parser "ac"),
    Test "parser alternatives commit after consuming input" $ do
      let parser = string "ab" <|> string "ac"
      result <- expectLeft "committed alternative" (runParser parser "ac")
      assertEqual "failure position" (SourcePos 1 1 2) (parseErrorPos result),
    Test "parser state tracks line and column" $ do
      result <- expectLeft "state position" (runParser (string "a\nb" <* eof) "a\n")
      assertEqual "newline position" (SourcePos 2 2 1) (parseErrorPos result),
    Test "parser combinators compose applicatively" $
      assertEqual "characters" (Right ('a', 'b')) (runParser ((,) <$> char 'a' <*> char 'b') "ab"),
    Test "reader parses atoms and quote" $
      assertEqual
        "quoted list"
        (Right (List [Symbol "quote", List [Number 1, StringLiteral "x\n", Boolean False]]))
        (readExpr "'(1 \"x\\n\" #f)"),
    Test "reader parses comments and multiple expressions" $
      assertEqual
        "program"
        (Right [List [Symbol "define", Symbol "x", Number 1], Symbol "x"])
        (readProgram "; setup\n(define x 1) ; value\nx"),
    Test "reader parses dotted lists" $
      assertEqual
        "dotted"
        (Right (DottedList [Symbol "a", Symbol "b"] (Symbol "c")))
        (readExpr "(a b . c)"),
    Test "reader distinguishes incomplete input" $ do
      err <- expectLeft "incomplete list" (readProgram "(+ 1\n")
      assertTrue "incomplete flag" (parseErrorIncomplete err),
    Test "reader rejects malformed tokens" $ do
      err <- expectLeft "bad token" (readExpr "123abc")
      assertTrue "invalid flag" (not (parseErrorIncomplete err)),
    Test "integer arithmetic and truncating division" $ do
      assertValues "arithmetic" ["7", "-3", "-3"] =<< evaluate "(+ 1 (* 2 3)) (/ -7 2) (- 3 6)",
    Test "only false is false" $
      assertValues "truth" ["yes", "no"] =<< evaluate "(if 0 'yes 'no) (if #f 'yes 'no)",
    Test "definitions and lexical closures" $
      assertValues "closure" ["ok", "ok", "8"]
        =<< evaluate "(define (make-adder x) (lambda (y) (+ x y))) (define add5 (make-adder 5)) (add5 3)",
    Test "set! updates a captured cell" $
      assertValues "mutation" ["ok", "ok", "11", "12"]
        =<< evaluate
          "(define x 1) (define counter ((lambda (x) (lambda () (begin (set! x (+ x 1)) x))) 10)) (counter) (counter)",
    Test "recursive procedures" $
      assertValues "factorial" ["ok", "720"]
        =<< evaluate "(define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1))))) (factorial 6)",
    Test "cond is expanded during analysis" $
      assertValues "cond" ["small", "other"]
        =<< evaluate "(cond ((< 2 3) 'small) (else 'large)) (cond (#f 1) (else 'other))",
    Test "quoted data becomes pairs" $
      assertValues "quoted pair" ["a", "(b . c)", "#t"]
        =<< evaluate "(car '(a b . c)) (cdr '(a b . c)) (equal? '(1 (2)) '(1 (2)))",
    Test "internal procedures can be mutually recursive" $
      assertValues "mutual recursion" ["#t"]
        =<< evaluate
          "((lambda (n) (define (even? x) (if (= x 0) #t (odd? (- x 1)))) (define (odd? x) (if (= x 0) #f (even? (- x 1)))) (even? n)) 20)",
    Test "internal definitions reject early reads" $
      assertEvaluationError "unassigned" "before initialization: y"
        "((lambda () (define x y) (define y 1) x))",
    Test "internal definitions must form a prefix" $
      assertEvaluationError "definition order" "must precede expressions"
        "((lambda () 1 (define x 2) x))",
    Test "runtime errors are structured" $ do
      assertEvaluationError "arity" "expected 2 argument(s), got 1" "(cons 1)"
      assertEvaluationError "type" "car: expected a pair" "(car 1)"
      assertEvaluationError "division" "division by zero" "(/ 1 0)"
      assertEvaluationError "unbound" "unbound variable: missing" "missing"
      assertEvaluationError "application" "non-procedure" "(1 2)",
    Test "analyze and execute are separate public operations" $ do
      expression <- expectRight "read" (readExpr "(+ 20 22)")
      executable <- expectRight "analyze" (analyze expression)
      env <- newGlobalEnv
      value <- execute env executable >>= expectRightIO "execute"
      assertEqual "analyzed value" "42" (renderValue value)
  ]

evaluate :: String -> IO [Value]
evaluate source = do
  env <- newGlobalEnv
  result <- runSource env source
  expectRightIO "evaluation" result

assertValues :: String -> [String] -> [Value] -> IO ()
assertValues label expected actual = assertEqual label expected (map renderValue actual)

assertEvaluationError :: String -> String -> String -> IO ()
assertEvaluationError label fragment source = do
  env <- newGlobalEnv
  result <- runSource env source
  case result of
    Left err -> assertTrue label (fragment `isInfixOf` renderInterpreterError err)
    Right values -> fail (label ++ ": expected an error, got " ++ show (map renderValue values))

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual
  | expected == actual = pure ()
  | otherwise = fail (label ++ ": expected " ++ show expected ++ ", got " ++ show actual)

assertTrue :: String -> Bool -> IO ()
assertTrue _ True = pure ()
assertTrue label False = fail (label ++ ": condition was false")

expectRight :: Show error => String -> Either error value -> IO value
expectRight _ (Right value) = pure value
expectRight label (Left err) = fail (label ++ ": " ++ show err)

expectRightIO :: Show error => String -> Either error value -> IO value
expectRightIO = expectRight

expectLeft :: Show value => String -> Either error value -> IO error
expectLeft _ (Left err) = pure err
expectLeft label (Right value) = fail (label ++ ": expected failure, got " ++ show value)
