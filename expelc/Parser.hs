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
        Base.BindValue _ -> False
      typeNodes = filter isTypeNode subs
    in if length typeNodes > 1
      then Left ("cannot give more than one type for binding " ++ name)
      else let
         bindType = if null typeNodes then Base.UnknownType
           else case head typeNodes of Base.BindType t -> t
         values = mapMaybe (\n -> case n of
           Base.BindValue nv -> Just nv
           Base.BindType _ -> Nothing) subs
        in Right $ Base.Binding name bindType values

  parseBindChild :: IndParser Base.BindChild
  parseBindChild = do
    P.spaces
    leader <- P.oneOf "^="
    P.spaces
    _ <- trace (show leader) return leader
    case leader of
      '^' -> Base.BindType <$> TypeParser.parseType
      '=' -> return $ Base.BindValue (Base.IntLiteral 42)

  parseBinding :: IndParser Base.Node
  parseBinding = do
    _ <- P.char ':'
    P.spaces
    bind <- I.withBlock makeBinding parseName parseBindChild
    case bind of
      Left errMsg -> fail errMsg
      Right node -> return node

  parse :: String -> Either P.ParseError Base.Node
  parse = indParse parseBinding "source"
