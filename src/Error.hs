module Error (
  Error (Error),
  formatErrorMessage,
) where

import Text.Printf (printf)

data Error = Error String Int deriving Eq
instance Show Error where
  show (Error msg pos) = printf "ERROR:%d:%s" pos msg

{-|
 -  Formats error message to display
 -  function accepts original text and Error instance and returns pretty
 -  formatted error message which points to exact error location in the original
 -  text:
 -
 -    Unexpected symbol '@' at position 2:
 -    a@ lorem ipsum
 -     ^
 -}
formatErrorMessage :: String -> Error -> String
formatErrorMessage text (Error msg pos) = 
  printf "%s at position %d\n%s\n%s" msg pos text pointer
  where pointer = replicate (pos - 1) ' ' ++ "^"
