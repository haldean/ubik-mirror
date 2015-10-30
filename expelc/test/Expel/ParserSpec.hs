module Expel.ParserSpec (spec) where

  import Test.Hspec
  import Expel.Base
  import Expel.Parser

  spec :: Spec
  spec = do
    describe "bindings" $ do
      it "parses basic bindings" $ do
        parseExpel ": lucky ^Int = 14" `shouldBe`
          Right [Binding "lucky" (BaseType "Int") [IntLiteral 14]]
