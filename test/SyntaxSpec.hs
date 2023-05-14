

module SyntaxSpec where

import Test.Hspec

import Text.Megaparsec (errorBundlePretty, parse)

import Types
import Operations
import Frontend.GenericAST
import Frontend.Parser.AST
import Frontend.Parser.Expr
import Frontend.Parser.Statement
import Frontend.Parser.Function hiding (params)
import Frontend.Parser.Program

spec :: Spec
spec =
    describe "parser tests" $ do
        describe "expressions" $ do
            it "parses a number" $ do
                let src = "2"
                let expected = Right ((Lit $ IntVal 2) 1)

                parse expr "" src `shouldBe` expected

            it "parses an addition" $ do
                let src = "2 + 3"
                let expected = Right (Arith Add 2 (Lit (IntVal 2) 2) (Lit (IntVal 3) 5))

                parse expr "" src `shouldBe` expected

            it "parses a call" $ do
                let src = "func()"
                let expected = Right (Call 0 "func" [])

                parse expr "" src `shouldBe` expected

        describe "statements" $ do
            it "parses a return" $ do
                let src = "return 0"
                let expected = Right (Return (Just (Lit (IntVal 0) 8)) 0)

                parse statement "" src `shouldBe` expected

            it "parses an if" $ do
                let src = "if (2 > 1) {\n print(\"obvio\") \n}"
                let expected = Right (If (Comp Gt 6 (Lit (IntVal 2) 6) (Lit (IntVal 1) 9)) [Print (Lit (StringVal "obvio") 27) 14] 0)

                parse statement "" src `shouldBe` expected
        
        describe "functions" $ do
            it "parses a function" $ do
                let src = "fun add(a int, b int) { return a + b }"
                let expected = Right (Function {funcName = "add", params = [Param {name = "a", type_ = IntType, parsePos = 8},Param {name = "b", type_ = IntType, parsePos = 15}], ret = ReturnSignature {typeSignature = Nothing, pos = 22}, stmts = [Return (Just (Arith Add 33 (SymbolRef 31 "a") (SymbolRef 35 "b"))) 24], position = 0})

                parse function "" src `shouldBe` expected

        describe "program" $ do
            it "parses a program with two functions" $ do
                let src = "fun nothing() {}\n fun nothing2() {}"
                let expected = Right [Function {funcName = "nothing", params = [], ret = ReturnSignature {typeSignature = Nothing, pos = 14}, stmts = [], position = 0},Function {funcName = "nothing2", params = [], ret = ReturnSignature {typeSignature = Nothing, pos = 33}, stmts = [], position = 18}]

                parse program "" src `shouldBe` expected