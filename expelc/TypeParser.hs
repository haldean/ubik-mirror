module TypeParser where
  import Base
  import ParseUtil
  import Text.Parsec
  import qualified Text.Parsec as Parsec

  parseTypeName :: IndParser Base.Type
  parseTypeName = do
    start <- Parsec.upper
    rest <- Parsec.many Parsec.alphaNum
    return $ Base.BaseType (start : rest)

  parseTypeVar :: IndParser Base.Type
  parseTypeVar = do
    start <- Parsec.lower
    rest <- Parsec.many Parsec.alphaNum
    return $ Base.VarType (start : rest)

  parseUnconstrainedType :: IndParser Base.Type
  parseUnconstrainedType = do
    base <- parseTypeName <|> parseTypeVar
    -- try to parse the remainder as a function
    Parsec.option base $
      Parsec.try (Base.FuncType base <$>
        (Parsec.spaces *> Parsec.string "->" *> Parsec.spaces *> parseUnconstrainedType))

  parseTypeConstraint :: IndParser Base.TypeConstraint
  parseTypeConstraint = do
    typeClass <- parseTypeName
    Parsec.spaces
    Base.VarType typeVar <- parseTypeVar
    return $ Base.TypeConstraint typeClass typeVar

  parseTypeConstraints :: IndParser [Base.TypeConstraint]
  parseTypeConstraints =
    Parsec.sepBy1 parseTypeConstraint $
      Parsec.spaces *> Parsec.char ',' *> Parsec.spaces

  parseType :: IndParser Base.Type
  parseType = do
    baseType <- parseUnconstrainedType
    -- try to parse a type constraint off the end
    Parsec.option baseType $
      Parsec.try (Base.ConstrainedType baseType <$>
        (Parsec.spaces *> Parsec.char '|' *> Parsec.spaces *> parseTypeConstraints))

  parseOptionalType :: IndParser Base.Type
  parseOptionalType = do
    caret <- Parsec.optionMaybe $ Just <$> Parsec.char '^'
    case caret of
      Nothing -> return Base.UnknownType
      Just _ -> Parsec.spaces *> parseType
