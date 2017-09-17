import Test.Hspec

import Error (Error(..))
import SyntaxChecker (checkParenParity, checkTokenOrder)
import Tokenizer (tokenize, Token(..), OpType(..), ParenType(..))

main :: IO ()
main = hspec $ do
  describe "Tokenizer.tokenize" $ do
    it "parses empty string to EOF" $ do
      tokenize "" `shouldBe` Right [TokEnd 0]
    it "can parse integers" $ do
      tokenize "12" `shouldBe` Right [TokConst 1 12, TokEnd 2]
    it "can parse floats" $ do
      tokenize "12.3" `shouldBe` Right [TokConst 1 12.3, TokEnd 4]
    it "errors on malformed floats" $ do
      tokenize ".0" `shouldBe` Left [Error "Unexpected symbol '.'" 1]
      tokenize "0." `shouldBe` Left [Error "Unexpected symbol '.'" 2]
      tokenize "0.0.1" `shouldBe` Left [Error "Unexpected symbol '.'" 4]
    it "can parse variables" $ do
      tokenize "a" `shouldBe` Right [TokVar 1 "a", TokEnd 1]
      tokenize "aLongVariable" `shouldBe` Right [TokVar 1 "aLongVariable"
                                                ,TokEnd 13]
    it "can parse operators" $ do
      tokenize "+-*/" `shouldBe` Right [TokOp 1 Add
                                       ,TokOp 2 Subtract
                                       ,TokOp 3 Multiply 
                                       ,TokOp 4 Divide
                                       ,TokEnd 4]
    it "can parse parens" $ do
      tokenize "()" `shouldBe` Right [TokParen 1 LParen
                                     ,TokParen 2 RParen
                                     ,TokEnd 2]
    it "errors on unexpected symbol" $ do
      tokenize "@" `shouldBe` Left [Error "Unexpected symbol '@'" 1]
      tokenize "99%" `shouldBe` Left [Error "Unexpected symbol '%'" 3]

  describe "SyntaxChecker.checkParenParity" $ do
    it "errors on unpaired parens" $ do
      (Right "(" >>= tokenize >>= checkParenParity) `shouldBe` 
        Left [Error "Unpaired paren (" 1]
      (Right ")" >>= tokenize >>= checkParenParity) `shouldBe` 
        Left [Error "Unpaired paren )" 1]
    it "passes paired parens" $ do
      (Right "(()())" >>= tokenize >>= checkParenParity) `shouldBe` 
        Right [TokParen 1 LParen, TokParen 2 LParen, TokParen 3 RParen
              ,TokParen 4 LParen, TokParen 5 RParen, TokParen 6 RParen
              ,TokEnd 6]
  describe "SyntaxChecker.checkTokenOrder" $ do
    it "forbids empty parens" $ do
      (Right "()" >>= tokenize >>= checkTokenOrder) `shouldBe`
        Left [Error "Empty parens are not allowed" 2]
    it "allows negatives on start" $ do
      (Right "-2" >>= tokenize >>= checkTokenOrder) `shouldBe`
        Right [TokOp 1 Subtract, TokConst 2 2, TokEnd 2]
      (Right "-var" >>= tokenize >>= checkTokenOrder) `shouldBe`
        Right [TokOp 1 Subtract, TokVar 2 "var", TokEnd 4]
    it "forbids operation on start" $ do
      (Right "+" >>= tokenize >>= checkTokenOrder) `shouldBe`
        Left [Error "Unexpected token `+`" 1, Error "Unexpected end of input" 1]
      (Right "*" >>= tokenize >>= checkTokenOrder) `shouldBe`
        Left [Error "Unexpected token `*`" 1, Error "Unexpected end of input" 1]
    it "forbids double operators" $ do
      (Right "2++3" >>= tokenize >>= checkTokenOrder) `shouldBe`
        Left [Error "Unexpected token `+`" 3]
      (Right "2+-3" >>= tokenize >>= checkTokenOrder) `shouldBe`
        Left [Error "Unexpected token `-`" 3]
