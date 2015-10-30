module Expel.Base where
  import Data.List
  import qualified Data.Text as T

  type Name = T.Text

  type TypeName = T.Text
  data TypeConstraint = TypeConstraint Type Name deriving (Eq)
  data Type = BaseType TypeName
            | VarType Name
            | FuncType Type Type
            | ConstrainedType Type [TypeConstraint]
            | UnknownType
            deriving (Eq)

  data Node = IntLiteral Integer
            | FloatLiteral Double
            | StringLiteral T.Text
            | Symbol Name
            | TypeNode Type
            | Apply Node Node
            | Binding Name Type [Node]
            | Prog Node Node
            | Func [Node] Node
            deriving (Eq)

  data BindChild = BindType Type | BindValue Node | BindTypeAndValue Type Node

  instance Show Node where
    show (IntLiteral i) = show i ++ "i"
    show (FloatLiteral f) = show f ++ "f"
    show (StringLiteral s) = "\"" ++ T.unpack s ++ "\""
    show (Symbol s) = "`" ++ T.unpack s
    show (Apply f v) = "(" ++ show f ++ " " ++ show v ++ ")"
    show (Binding n t body) = ":" ++ T.unpack n ++ " ^(" ++ show t ++ ") =(" ++ show body ++ ")"
    show (Prog n1 n2) = show n1 ++ "\n" ++ show n2
    show (Func args body) = "\\" ++ show args ++ " -> " ++ show body

  instance Show Type where
    show (BaseType name) = T.unpack name
    show (VarType name) = "$" ++ T.unpack name
    show (FuncType from to) = show from ++ " -> " ++ show to
    show (ConstrainedType base constraints) =
      show base ++ " | " ++ intercalate ", " (map show constraints)
    show (UnknownType) = "(?)"

  instance Show TypeConstraint where
    show (TypeConstraint cls name) = show cls ++ " " ++ T.unpack name
