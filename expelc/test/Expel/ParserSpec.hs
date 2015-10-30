module Expel.ParserSpec (spec) where

  import Test.Hspec
  import qualified Data.Text as T
  import Expel.Base
  import Expel.Parser

  spec :: Spec
  spec =
    describe "bindings" $ do

      it "parses basic bindings" $
        parseExpel ": lucky ^Int = 14" `shouldBe`
          Right [Binding "lucky" (BaseType "Int") [IntLiteral 14]]

      it "parses complex types" $
        parseExpel ":testFunc ^ a -> Int -> a | Num a = undefined" `shouldBe`
          Right [Binding
            "testFunc"
            (ConstrainedType
              (FuncType
                (VarType "a")
                (FuncType (BaseType "Int") (VarType "a"))
              )
              [TypeConstraint (BaseType "Num") "a"]
            )
            [Symbol "undefined"]
            ]

      it "parses multiline bindings" $ do
        let prog = T.unlines [": test",
                              "  ^ String -> Int -> Int",
                              "  = λ x y ->",
                              "    +",
                              "      x y",
                              "  = \\x y -> 7"
                              ]
        parseExpel prog `shouldBe` Right [Binding
            "test"
            (FuncType (BaseType "String") (FuncType (BaseType "Int") (BaseType "Int")))
            [
              Func [Symbol "x", Symbol "y"] (Apply (Apply (Symbol "+") (Symbol "x")) (Symbol "y")),
              Func [Symbol "x", Symbol "y"] (IntLiteral 7)
            ]
          ]

      it "doesn't allow conflicting type signatures" $ do
        let prog = T.unlines [": test",
                              "  ^ String -> Int -> Int",
                              "  ^ Int -> Int -> String",
                              "  = \\x y -> 7"
                              ]
        parseExpel prog `shouldSatisfy` \x -> case x of
          Left _ -> True
          Right _ -> False
