module Expel where
  import Data.List

  type Name = String

  type TypeName = String
  data TypeConstraint = TypeConstraint Type Name
  data Type = BaseType TypeName
            | VarType Name
            | FuncType Type Type
            | ConstrainedType Type [TypeConstraint]
            | UnknownType

  data Node = IntLiteral Integer
            | FloatLiteral Double
            | StringLiteral String
            | Symbol String
            | Apply Node Node
            | Binding Name Type Node
            | Prog Node Node

  instance Show Node where
    show (IntLiteral i) = show i
    show (FloatLiteral f) = show f
    show (StringLiteral s) = "\"" ++ s ++ "\""
    show (Symbol s) = "." ++ s ++ "."
    show (Apply f v) = "(" ++ show f ++ " " ++ show v ++ ")"
    show (Binding n t body) = ":" ++ n ++ " ^(" ++ show t ++ ") =(" ++ show body ++ ")"
    show (Prog n1 n2) = show n1 ++ "\n" ++ show n2

  instance Show Type where
    show (BaseType name) = name
    show (VarType name) = "$" ++ name
    show (FuncType from to) = show from ++ " -> " ++ show to
    show (ConstrainedType base constraints) =
      show base ++ " | " ++ intercalate ", " (map show constraints)
    show (UnknownType) = "(?)"

  instance Show TypeConstraint where
    show (TypeConstraint cls name) = show cls ++ " " ++ name
