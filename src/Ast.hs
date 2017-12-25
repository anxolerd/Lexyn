module Ast (
    Ast (AstVar, AstConst, AstOp),
    buildAst,
) where

import Tokenizer (OpType (..), Token (..), ParenType (..), tokenPos)
import Error (Error (..))
import Text.Printf

type Precedence = Int
data Ast = AstVar String
         | AstConst Double
         | AstOp OpType Ast Ast

instance Show Ast where
    show = showAtLevel 0
        where showAtLevel l (AstVar name) = (addSpace l) ++ name
              showAtLevel l (AstConst val) = (addSpace l) ++ (show val)
              showAtLevel l (AstOp op left right) =
                printf "%s%s\n%s\n%s\n"
                       (addSpace l) (show op)
                       (showAtLevel (l + 1) left)
                       (showAtLevel (l+1) right)
              addSpace = flip replicate '\t'


operatorPrecedence :: OpType -> Precedence
operatorPrecedence op
    | op `elem` [Add, Subtract] = 0
    | op `elem` [Multiply, Divide] = 2
minimumPrecedence = 0


isExpr :: Token -> Bool
isExpr (TokOp _ Add) = True
isExpr (TokOp _ Subtract) = True
isExpr _ = False


isTerm :: Token -> Bool
isTerm (TokOp _ Multiply) = True
isTerm (TokOp _ Divide) = True
isTerm _ = False


parseExpr :: [Token] -> (Ast, [Token])
parseExpr tokens = parseExpr' . parseTerm $ tokens
parseExpr' :: (Ast, [Token]) -> (Ast, [Token])
parseExpr' (lhs, []) = (lhs, [])
parseExpr' (lhs, lookahead:tokens)
    | isExpr lookahead = let (rhs, tokens') = parseTerm tokens
                             (TokOp _ opType) = lookahead
                         in parseExpr' (AstOp opType lhs rhs, tokens')
    | otherwise = (lhs, lookahead:tokens)


parseTerm :: [Token] -> (Ast, [Token])
parseTerm tokens =  parseTerm' . parseFactor $ tokens
parseTerm' (lhs, lookahead:tokens)
    | isTerm lookahead = let (rhs, tokens') = parseFactor tokens
                             (TokOp _ opType) = lookahead
                         in parseTerm' (AstOp opType lhs rhs, tokens')
    | otherwise = (lhs, lookahead:tokens)

parseFactor :: [Token] -> (Ast, [Token])
parseFactor (TokConst _ val:tokens) = (AstConst val, tokens)
parseFactor (TokVar _ name:tokens) = (AstVar name, tokens)
parseFactor (TokParen _ LParen:tokens) =
    let (ast, tokens') = parseExpr tokens
    in (ast, drop 1 tokens')
parseFactor (TokOp _ Subtract:tokens) =
    let (ast, tokens') = parseFactor tokens
    in case ast of
        (AstConst val) -> (AstConst (- val), tokens')
        token -> (AstOp Multiply (AstConst (-1)) ast, tokens')

{-|
 - FIXME: this builds quite deep and narrow tree,
 - which is bad for parallelization
 -}
buildAst :: [Token] -> Either [Error] Ast
buildAst tokens = case parseExpr tokens of
    (tree, [(TokEnd _)]) -> Right tree
    (_, leftover) -> let msg = "Unexpected leftover: " ++ (show leftover)
                         pos = tokenPos $ head leftover
                     in  Left [(Error msg pos)]
