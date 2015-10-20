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

  data Node = IntLiteral Int
            | FloatLiteral Float
            | StringLiteral String
            | Apply Node Node
            | Binding Name Type Node

  instance Show Type where
    show (BaseType name) = name
    show (VarType name) = "$" ++ name
    show (FuncType from to) = show from ++ " -> " ++ show to
    show (ConstrainedType base constraints) =
      show base ++ " | " ++ intercalate ", " (map show constraints)
    show (UnknownType) = "(?)"

  instance Show TypeConstraint where
    show (TypeConstraint cls name) = show cls ++ " " ++ name
