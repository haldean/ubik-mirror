module Parser where
  import ParseUtil
  import Data.Maybe
  import Text.Parsec ( (<|>) )
  import qualified Base
  import qualified Control.Monad as M
  import qualified Data.Text as T
  import qualified TypeParser
  import qualified Text.Parsec as P
  import qualified Text.Parsec.Indent as I

  parseName :: IndParser Base.Name
  parseName = T.pack <$> P.many1 (P.alphaNum <|> P.oneOf "!@#$%&*-_+='|><.?:/")

  parseSymbol :: IndParser Base.Node
  parseSymbol = Base.Symbol <$> parseName

  makeBinding :: Base.Name -> [Base.BindChild] -> Either String Base.Node
  makeBinding name subs =
    let
      isTypeNode n = case n of
        Base.BindType _ -> True
        Base.BindTypeAndValue _ _ -> True
        Base.BindValue _ -> False
      typeNodes = filter isTypeNode subs
    in if length typeNodes > 1
      then Left ("cannot give more than one type for binding \"" ++ show name ++ "\"")
      else let
         bindType = if null typeNodes then Base.UnknownType
           else case head typeNodes of
             Base.BindType t -> t
             Base.BindTypeAndValue t _ -> t
         values = mapMaybe (\n -> case n of
           Base.BindValue nv -> Just nv
           Base.BindTypeAndValue _ nv -> Just nv
           Base.BindType _ -> Nothing) subs
        in Right $ Base.Binding name bindType values

  parseFuncArg :: IndParser Base.Node
  parseFuncArg = Base.Symbol . T.pack <$> P.many1 P.alphaNum

  parseFunc :: IndParser Base.Node
  parseFunc = do
    P.try (P.oneOf "\\λ")
    P.spaces
    args <- P.many (checkIndent' >> parseFuncArg <* P.spaces)
    _ <- P.string "->"
    P.spaces
    body <- parseExpr
    return $ Base.Func args body

  parseApply :: IndParser Base.Node
  parseApply = P.manyTill parseSimpleExpr (checkIndentLess <|> P.eof) >>=
    \exprs -> if null exprs
      then fail "parseApply got less than two applications"
      else return $ foldl1 Base.Apply exprs

  parseSimpleExpr :: IndParser Base.Node
  parseSimpleExpr = P.spaces >> (
    parseNumber <|>
    parseSymbol <|>
    (P.char '(' *> parseExpr <* P.char ')')) <* P.spaces

  parseNumber :: IndParser Base.Node
  parseNumber = P.try (do
    pre <- P.many1 P.digit
    period <- P.optionMaybe (P.oneOf ".e")
    case period of
      Nothing -> P.space >> return (Base.IntLiteral (read pre :: Integer))
      Just 'e' -> P.many P.digit >>= \exps ->
        P.space >> return (Base.FloatLiteral (read (pre ++ "e" ++ exps) :: Double))
      Just '.' -> do
        frac <- P.many P.digit
        exps <- P.option "" (P.char 'e' >> P.many P.digit)
        P.space
        return $ Base.FloatLiteral
          (read (pre ++ "." ++ frac ++ "e" ++ exps) :: Double))

  parseExpr :: IndParser Base.Node
  parseExpr = P.spaces >> (parseFunc <|> parseApply <|> parseNumber <|> parseSymbol)

  parseBindChild :: IndParser Base.BindChild
  parseBindChild = do
    P.spaces
    leader <- P.oneOf "^="
    case leader of
      '^' -> do
        P.spaces
        bindType <- TypeParser.parseType
        _ <- P.many (P.oneOf " \t")
        eq <- P.try (P.optionMaybe $ P.char '=')
        P.spaces
        case eq of
          Just _ -> Base.BindTypeAndValue bindType <$> parseExpr
          Nothing -> return (Base.BindType bindType)
      '=' -> I.withPos $ P.spaces >> Base.BindValue <$> parseExpr

  parseBinding :: IndParser Base.Node
  parseBinding = do
    _ <- P.char ':'
    P.spaces
    name <- parseName
    P.spaces
    children <- I.withPos $ I.block parseBindChild
    let bind = makeBinding name children in
      case bind of
        Left errMsg -> fail errMsg
        Right node -> return node

  parseFile :: IndParser [Base.Node]
  parseFile = P.many parseBinding

  parse :: T.Text -> Either P.ParseError [Base.Node]
  parse = indParse parseFile "source"
