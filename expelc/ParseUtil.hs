module ParseUtil where
  import qualified Text.Parsec as P
  import qualified Control.Monad.State as S
  import qualified Text.Parsec.Indent as I

  -- many thanks to @sw17ch for sharing these little bits on his blog.
  type IndParser a = P.ParsecT String () (S.State P.SourcePos) a

  indParse :: IndParser a -> P.SourceName -> String -> Either P.ParseError a
  indParse p source_name input =
    I.runIndent source_name $ P.runParserT p () source_name input
