module Expel.TypeParser where
  import qualified Expel.Base as Base
  import Expel.ParseUtil
  import qualified Data.Text as T
  import Text.Parsec ( (<|>) )
  import qualified Text.Parsec as P

  parseTypeName :: IndParser Base.Type
  parseTypeName = do
    start <- P.upper
    rest <- P.many P.alphaNum
    return $ Base.BaseType $ T.pack (start : rest)

  parseTypeVar :: IndParser Base.Type
  parseTypeVar = do
    start <- P.lower
    rest <- P.many P.alphaNum
    return $ Base.VarType $ T.pack (start : rest)

  parseUnconstrainedType :: IndParser Base.Type
  parseUnconstrainedType = do
    base <- parseTypeName <|> parseTypeVar
    -- try to parse the remainder as a function
    P.option base $
      P.try (Base.FuncType base <$>
        (P.spaces *> P.string "->" *> P.spaces *> parseUnconstrainedType))

  parseTypeConstraint :: IndParser Base.TypeConstraint
  parseTypeConstraint = do
    typeClass <- parseTypeName
    P.spaces
    Base.VarType typeVar <- parseTypeVar
    return $ Base.TypeConstraint typeClass typeVar

  parseTypeConstraints :: IndParser [Base.TypeConstraint]
  parseTypeConstraints =
    P.sepBy1 parseTypeConstraint $
      P.spaces *> P.char ',' *> P.spaces

  parseType :: IndParser Base.Type
  parseType = do
    baseType <- parseUnconstrainedType
    -- try to parse a type constraint off the end
    P.option baseType $
      P.try (Base.ConstrainedType baseType <$>
        (P.spaces *> P.char '|' *> P.spaces *> parseTypeConstraints))

  parseOptionalType :: IndParser Base.Type
  parseOptionalType = do
    caret <- P.optionMaybe $ Just <$> P.char '^'
    case caret of
      Nothing -> return Base.UnknownType
      Just _ -> P.spaces *> parseType
