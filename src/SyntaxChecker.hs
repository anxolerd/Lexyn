module SyntaxChecker (
  check
) where

import Text.Printf (printf)

import Error (Error (..))
import Stack (Stack (..), newStack, push, pop, toList, isEmpty)
import Tokenizer (Token (..), ParenType (..), OpType (..))


{-|
  The 'checkParenParity' function checks whether all parens in the
  input sequence are paired. If there are unpaired parens, the 
  function will return the list of errors, otherwise the input sequence
-}
checkParenParity :: [Token] -> Either [Error] [Token]
checkParenParity tokens = case check tokens newStack [] of
  [] -> Right tokens
  errorTokens -> Left $ map toError errorTokens
  where
    check :: [Token] -> Stack Token -> [Token] -> [Token]
    check [] stack errorTokens = 
      if isEmpty stack then errorTokens' else errorTokens' ++ toList stack
      where errorTokens' = reverse errorTokens
    check (t:ts) stack errorTokens = case t of
      TokParen _ LParen -> check ts (push stack t) errorTokens
      TokParen _ RParen -> case pop stack of
        (Nothing, stack') -> check ts stack' (t:errorTokens)
        (_, stack') -> check ts stack' errorTokens
      _ -> check ts stack errorTokens

    toError :: Token -> Error
    toError (TokParen pos paren) = 
      Error (printf "Unpaired paren %s" (show paren)) pos

{-
 - begin
 - end
 - begin -> - -> negate
 - begin -> 1 -> num
 - begin -> ( -> lparen
 - begin -> op -> error
 - begin -> ) -> error
 - begin -> $ -> error
 - negate -> 1 -> num
 - negate -> ( -> lparen
 - negate -> op -> error
 - negate -> ) -> error
 - negate -> $ -> error
 - num -> op -> operator
 - num -> ) -> rparen
 - num -> ( -> error
 - num -> 1 -> error
 - num -> $ -> end
 - lparen -> ( -> lparen
 - lparen -> - -> negate
 - lparen -> 1 -> num
 - lparen -> op -> error
 - lparen -> ) -> error
 - lparen -> $ -> error
 - operator -> ( -> lparen
 - operator -> 1 -> num
 - operator -> op -> error
 - operator -> ) -> error
 - operator -> $ -> error
 - rparen -> op -> operator
 - rparen -> 1 -> error
 - rparen -> ( -> error
 - rparen -> ) -> rparen
 - rparen -> $ -> end
 - end -> * -> error
 -}
data CheckerState = SBegin
                  | SEnd
                  | SNegate
                  | SOperator
                  | SNum
                  | SLparen
                  | SRparen
  deriving (Show, Eq)

unexpectedTokenMsg :: String -> String
unexpectedTokenMsg = printf "Unexpected token `%s`"

unexpectedToken :: Token -> Error
unexpectedToken (TokOp pos op) = Error (unexpectedTokenMsg (show op)) pos
unexpectedToken (TokParen pos paren) = Error (unexpectedTokenMsg (show paren)) pos
unexpectedToken (TokConst pos value) = Error (unexpectedTokenMsg (show value)) pos
unexpectedToken (TokVar pos name) = Error (unexpectedTokenMsg (show name)) pos
unexpectedToken (TokEnd pos) = Error "Unexpected end of input" pos

emptyParens :: Int -> Error
emptyParens = Error (printf "Empty parens are not allowed")

checkTokenOrder :: [Token] -> Either [Error] [Token]
checkTokenOrder tokens = case check SBegin tokens [] of
  [] -> Right tokens
  errors -> Left $ reverse errors
  where 
    check :: CheckerState -> [Token] -> [Error] -> [Error]
    check _ [] errors = errors
    check state (t:ts) errors = case accept state t of
      (newState, Nothing) -> check newState ts errors
      (newState, Just e) -> check newState ts (e:errors)
    accept :: CheckerState -> Token -> (CheckerState, Maybe Error)
    accept SBegin (TokOp _ Subtract) = (SNegate, Nothing)
    accept SBegin (TokConst _ _) = (SNum, Nothing)
    accept SBegin (TokVar _ _) = (SNum, Nothing)
    accept SBegin (TokParen _ LParen) = (SLparen, Nothing)
    accept SBegin t = (SBegin, Just $ unexpectedToken t)
    accept SNegate (TokConst _ _) = (SNum, Nothing)
    accept SNegate (TokVar _ _) = (SNum, Nothing)
    accept SNegate (TokParen _ LParen) = (SLparen, Nothing)
    accept SNegate t = (SNegate, Just $ unexpectedToken t)
    accept SNum (TokOp _ _) = (SOperator, Nothing)
    accept SNum (TokParen _ RParen) = (SRparen, Nothing)
    accept SNum (TokEnd _) = (SEnd, Nothing)
    accept SNum t = (SNum, Just $ unexpectedToken t)
    accept SLparen (TokParen _ LParen) = (SLparen, Nothing)
    accept SLparen (TokOp _ Subtract) = (SNegate, Nothing)
    accept SLparen (TokVar _ _) = (SNum, Nothing)
    accept SLparen (TokConst _ _) = (SNum, Nothing)
    accept SLparen (TokParen pos RParen) = (SRparen, Just $ emptyParens pos)
    accept SLparen t = (SLparen, Just $ unexpectedToken t)
    accept SOperator (TokParen _ LParen) = (SLparen, Nothing)
    accept SOperator (TokConst _ _) = (SNum, Nothing)
    accept SOperator (TokVar _ _) = (SNum, Nothing)
    accept SOperator t = (SOperator, Just $ unexpectedToken t)    
    accept SRparen (TokOp _ _) = (SOperator, Nothing)
    accept SRparen (TokEnd _) = (SEnd, Nothing)
    accept SRparen t = (SRparen, Just $ unexpectedToken t)
    accept SEnd t = (SEnd, Just $ unexpectedToken t)

check :: [Token] -> Either [Error] [Token]
check tokens = Right tokens >>= checkParenParity >>= checkTokenOrder
