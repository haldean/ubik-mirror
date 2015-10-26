module Tokenize where
  import Expel
  import TypeParser
  import Text.Parsec
  import qualified Data.Text as T
  import qualified Text.Parsec as Parsec
  import qualified Text.Parsec.Language as Lang
  import qualified Text.Parsec.Token as Token

  lexer :: Token.TokenParser ()
  lexer = Token.makeTokenParser style
    where style = Lang.emptyDef {
    Token.commentLine = "//"
    }

  takeNum :: Parsec T.Text () Expel.Node
  takeNum = fmap toExpelNode (Token.naturalOrFloat lexer) where
    toExpelNode x = case x of
      Left intVal -> Expel.IntLiteral intVal
      Right floatVal -> Expel.FloatLiteral floatVal

  takeSymbol :: Parsec T.Text () Expel.Node
  takeSymbol = Expel.Symbol <$> Token.identifier lexer
