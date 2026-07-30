module Main (main) where

import Control.Exception (IOException, try)
import Scheme
  ( newGlobalEnv,
    renderInterpreterError,
    renderValue,
    runSource,
  )
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  arguments <- getArgs
  case arguments of
    [path] -> runFile path
    _ -> do
      putStrLn "usage: sicp <file.scm>"
      exitFailure

runFile :: FilePath -> IO ()
runFile path = do
  contentResult <- try (readFile path) :: IO (Either IOException String)
  case contentResult of
    Left err -> do
      putStrLn (path ++ ": " ++ show err)
      exitFailure
    Right source -> do
      env <- newGlobalEnv
      result <- runSource env source
      case result of
        Left err -> do
          putStrLn (path ++ ": " ++ renderInterpreterError err)
          exitFailure
        Right values -> mapM_ (putStrLn . renderValue) values
