module Main where

import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)

import Tokenizer (tokenize)
import SyntaxChecker (check)
import Error (formatErrorMessage)

main :: IO ()
main = do
    args <- getArgs
    let expression = concat args
    let formatter = formatErrorMessage expression
    case Right expression >>= tokenize >>= check of
      Left errors -> do
        mapM_ (putStrLn . formatter) errors
        exitFailure
      Right _ -> exitSuccess
