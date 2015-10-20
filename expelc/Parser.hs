module Parser where

  import Debug.Trace
  import Expel
  import Text.Parsec
  import qualified Text.Parsec as Parsec

  parseName :: Parsec String () String
  parseName = Parsec.many1 Parsec.alphaNum

  parseTypeName :: Parsec String () Expel.Type
  parseTypeName = do
    start <- Parsec.upper
    rest <- Parsec.many Parsec.alphaNum
    return $ Expel.BaseType (start : rest)

  parseTypeVar :: Parsec String () Expel.Type
  parseTypeVar = do
    start <- Parsec.lower
    rest <- Parsec.many Parsec.alphaNum
    return $ Expel.VarType (start : rest)

  parseUnconstrainedType :: Parsec String () Expel.Type
  parseUnconstrainedType = do
    base <- Parsec.try parseTypeName <|> parseTypeVar
    -- try to parse the remainder as a function
    Parsec.option base $
      Parsec.try (Expel.FuncType base <$>
        (Parsec.spaces *> Parsec.string "->" *> Parsec.spaces *> parseUnconstrainedType))

  parseTypeConstraint :: Parsec String () Expel.TypeConstraint
  parseTypeConstraint = do
    typeClass <- parseTypeName
    Parsec.spaces
    Expel.VarType typeVar <- parseTypeVar
    return $ Expel.TypeConstraint typeClass typeVar

  parseTypeConstraints :: Parsec String () [Expel.TypeConstraint]
  parseTypeConstraints =
    Parsec.sepBy1 parseTypeConstraint $
      Parsec.spaces *> Parsec.char ',' *> Parsec.spaces

  parseType :: Parsec String () Expel.Type
  parseType = do
    baseType <- parseUnconstrainedType
    -- try to parse a type constraint off the end
    Parsec.option baseType $
      Parsec.try (Expel.ConstrainedType baseType <$>
        (Parsec.spaces *> Parsec.char '|' *> Parsec.spaces *> parseTypeConstraints))

  parseOptionalType :: Parsec String () Expel.Type
  parseOptionalType = do
    caret <- Parsec.optionMaybe $ Parsec.try (Just <$> Parsec.char '^')
    case caret of
      Nothing -> return Expel.UnknownType
      Just _ -> Parsec.spaces *> parseType

  stringUntilEmpty :: Parsec String () String
  stringUntilEmpty = undefined

  parseBinding :: Parsec String () Expel.Node
  parseBinding = do
    _ <- Parsec.char ':'
    name <- parseName
    bindtype <- parseOptionalType
    return (Binding name bindtype undefined)

  parse :: Parsec String () [Expel.Node]
  parse = do
    result <- Parsec.many parseBinding
    _ <- Parsec.eof
    return result
