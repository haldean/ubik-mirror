module Parser where
  import Expel
  import TypeParser
  import Text.Parsec
  import qualified Text.Parsec as Parsec
  import qualified Text.Parsec.Language as Lang
  import qualified Text.Parsec.Token as Token

  parseName :: Parsec String () String
  parseName = Parsec.many1 (Parsec.alphaNum <|> Parsec.oneOf "!@#$%&*-_+='|><.?:/")

  lexer :: Token.TokenParser ()
  lexer = Token.makeTokenParser style
    where style = Lang.emptyDef {
    Token.commentLine = "//"
    }

  parseNum :: Parsec String () Expel.Node
  parseNum = fmap toExpelNode (Token.naturalOrFloat lexer) where
    toExpelNode x = case x of
      Left intVal -> Expel.IntLiteral intVal
      Right floatVal -> Expel.FloatLiteral floatVal

  parseSymbol :: Parsec String () Expel.Node
  parseSymbol = Expel.Symbol <$> Token.identifier lexer

  parseExpr :: Parsec String () Expel.Node
  parseExpr = (parseNum <|> parseSymbol)
    <* (Parsec.many1 Parsec.space <|>
        -- need the const [] here to make Parsec.eof match [Char] from many1
        (const [] <$> Parsec.eof))

  parseBinding :: Parsec String () Expel.Node
  parseBinding = do
    _ <- Parsec.char ':'
    _ <- Parsec.spaces
    name <- parseName
    _ <- Parsec.spaces
    bindtype <- parseOptionalType
    _ <- Parsec.spaces
    _ <- Parsec.char '='
    val <- parseExpr
    return (Binding name bindtype val)

  parse :: Parsec String () [Expel.Node]
  parse = do
    result <- Parsec.many parseBinding
    _ <- Parsec.eof
    return result
