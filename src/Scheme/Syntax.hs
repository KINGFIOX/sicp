module Scheme.Syntax
  ( Name,
    SExpr (..),
  )
where

type Name = String

data SExpr
  = Number Integer
  | StringLiteral String
  | Boolean Bool
  | Symbol Name
  | List [SExpr]
  | DottedList [SExpr] SExpr
  deriving (Eq, Show)
