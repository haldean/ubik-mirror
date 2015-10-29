module Parser where
  import ParseUtil
  import Data.Maybe
  import Debug.Trace
  import qualified Base
  import qualified TypeParser
  import qualified Text.Parsec as P
  import qualified Control.Monad.State as S
  import qualified Text.Parsec.Indent as I

  parseName :: IndParser Base.Name
  parseName = P.many1 (P.alphaNum P.<|> P.oneOf "!@#$%&*-_+='|><.?:/")

  makeBinding :: Base.Name -> [Base.BindChild] -> Either String Base.Node
  makeBinding name subs =
    let
      isTypeNode n = case n of
        Base.BindType _ -> True
        Base.BindTypeAndValue _ _ -> True
        Base.BindValue _ -> False
      typeNodes = filter isTypeNode subs
    in if length typeNodes > 1
      then Left ("cannot give more than one type for binding " ++ name)
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

  parseExpr :: IndParser Base.Node
  parseExpr = Base.StringLiteral . foldl1 (++) <$> contBlock (P.many (P.noneOf "\n") <* P.spaces)

  parseBindChild :: IndParser Base.BindChild
  parseBindChild = do
    P.spaces
    leader <- P.oneOf "^="
    P.spaces
    case leader of
      '^' -> do
        bindType <- TypeParser.parseType
        P.many (P.oneOf " \t")
        eq <- P.try (P.optionMaybe $ P.char '=')
        P.spaces
        case eq of
          Just _ -> Base.BindTypeAndValue bindType <$> parseExpr
          Nothing -> return (Base.BindType bindType)
      '=' -> Base.BindValue <$> parseExpr

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

  parse :: String -> Either P.ParseError Base.Node
  parse = indParse parseBinding "source"
