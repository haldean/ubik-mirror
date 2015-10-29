{-# LANGUAGE FlexibleContexts #-}

module ParseUtil where
  import qualified Control.Concatenative as C
  import qualified Control.Monad as M
  import qualified Control.Monad.State as S
  import qualified Data.Text as T
  import qualified Text.Parsec as P
  import qualified Text.Parsec.Indent as I

  -- many thanks to @sw17ch for sharing these little bits on his blog.
  type IndParser a = P.ParsecT T.Text () (S.State P.SourcePos) a

  indParse :: IndParser a -> P.SourceName -> T.Text -> Either P.ParseError a
  indParse p source_name input =
    I.runIndent source_name $ P.runParserT p () source_name input

  -- slightly modified from the source of the indents package; this checks that
  -- the current indentation level is greater than or equal to the stored position
  checkIndent' :: (P.Stream s (S.State P.SourcePos) z) => I.IndentParser s u ()
  checkIndent' = do
    s <- S.get
    p <- P.getPosition
    M.unless (C.biAp P.sourceColumn (>=) p s) $ P.parserFail "indentation doesn't match"

  -- like block, but continues parsing as long as the indentation level is equal
  -- or greater than the starting position.
  contBlock :: (P.Stream s (S.State P.SourcePos) z) => I.IndentParser s u a -> I.IndentParser s u [a]
  contBlock p = I.withPos $ P.many1 (checkIndent' >> p)
