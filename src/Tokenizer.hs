module Tokenizer (
  Position,
  OpType (Add, Subtract, Multiply, Divide),
  ParenType(LParen, RParen),
  Token(TokOp, TokParen, TokConst, TokVar, TokEnd),
  tokenize,
) where
import Data.Char (isSpace, isAlpha, isAlphaNum, isDigit)
import Text.Printf (printf)

import Error (Error (..))

type Position = Int
type Err = (String, Position)

data OpType = Add | Subtract | Multiply | Divide deriving (Eq)
instance Show OpType where
  show Add = "+"
  show Subtract = "-"
  show Multiply = "*"
  show Divide = "/"

operator :: Char -> OpType
operator '+' = Add
operator '-' = Subtract
operator '*' = Multiply
operator '/' = Divide

data ParenType = LParen | RParen deriving (Eq)
instance Show ParenType where
  show LParen = "("
  show RParen = ")"

paren :: Char -> ParenType
paren '(' = LParen
paren ')' = RParen

data Token = TokOp Position OpType
           | TokParen Position ParenType
           | TokConst Position Double
           | TokVar Position String
           | TokEnd Position -- Synthetic token
  deriving (Eq)

fmtToken :: String -> String -> Position -> String
fmtToken = printf "<%s %s at %d>"

instance Show Token where
  show (TokOp pos op) = fmtToken "Op" (show op) pos
  show (TokParen pos paren) = fmtToken "Paren" (show paren) pos
  show (TokConst pos value) = fmtToken "Const" (show value) pos
  show (TokVar pos name) = fmtToken "Var" (show name) pos
  show (TokEnd pos) = fmtToken "EOF" "$" pos

msgUnexpectedSymbol :: Char -> String
msgUnexpectedSymbol = printf "Unexpected symbol '%c'"

tokenize :: String -> Either [Error] [Token]
tokenize cs = case _tokenize (zip cs [1..]) [] [] of
  (tokens, []) -> Right $ reverse (TokEnd (length cs):tokens)
  (_, errors) -> Left $ reverse errors


_tokenize :: [(Char, Int)] -> [Token] -> [Error] -> ([Token], [Error])
_tokenize [] tokens errors = (tokens, errors)
_tokenize ((c, pos):cs) tokens errors
  | isSpace c       = _tokenize cs tokens errors
  | c `elem` "+-*/" = _tokenize cs (TokOp pos (operator c) : tokens) errors
  | isAlpha c       = word ((c, pos):cs) tokens errors
  | isDigit c       = number ((c, pos):cs) tokens errors
  | c `elem` "()"   = _tokenize cs (TokParen pos (paren c) : tokens) errors
  | otherwise       = _tokenize cs tokens (Error (msgUnexpectedSymbol c) pos : errors)

word :: [(Char, Int)] -> [Token] -> [Error] -> ([Token], [Error])
word cs tokens = _tokenize cs' (TokVar pos name : tokens)
  where
    pos          = snd . head $ cs
    (name', cs') = span (isAlphaNum . fst) cs
    name         = foldr ((:) . fst) "" name'

number :: [(Char, Int)] -> [Token] -> [Error] -> ([Token], [Error])
number cs tokens errors =
  let (digits, cs') = span (\(x, _) -> isAlphaNum x || x == '.') cs in
    if (fst . last) digits == '.' then
      _tokenize cs' tokens
                (Error (msgUnexpectedSymbol '.') (lastPos digits):errors)
    else let decimalPoints = filter isPoint digits in
      if length decimalPoints > 1 then
        _tokenize cs' tokens
                  (Error (msgUnexpectedSymbol '.') (lastPos decimalPoints):errors)
      else let value = read $ foldr ((:) . fst) "" digits in
        _tokenize cs' (TokConst pos value : tokens) errors
  where
    pos = (snd . head) cs
    isPoint (x, _) = x == '.'
    lastPos = snd . last
