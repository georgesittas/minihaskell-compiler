module Types where

type Identifier = String
type NumberLiteral = Int
type ResultExpression = Expression

-- AST representation of the source language (a small subset of Haskell).
data Program =  Program ResultExpression [FunctionDef] 
        deriving (Eq, Show)

-- Example: the function definition "foo(x, y) = x + y" is parsed into
-- FunctionInfo "foo" ["x", "y"] (BinaryOp Plus (ExprIdentifier "x") (ExprIdentifier "y")).
data FunctionDef =  FunctionInfo Identifier [Identifier] Expression 
        deriving (Eq, Show)

data Expression =   ExprIdentifier Identifier
                |   ExprNum NumberLiteral
                |   ExprBool Bool
                |   ExprParens Expression
                |   IfThenElse Expression Expression Expression
                |   Call Identifier [Expression]
                |   CompOp   OpCompare Expression Expression
                |   BinaryOp OpBinary Expression Expression
                |   BooleanOP OpBool Expression Expression
                |   UnaryOp  OpUnary  Expression

        deriving (Eq, Show)

data OpBinary = Plus | Mult | Minus | Div
        deriving (Eq, Show)

data OpCompare = OpLTEQ | OpLT | OpGTEQ | OpGT | OpEQ | OpNEQ
        deriving (Eq, Show)

data OpBool = And | Or
        deriving (Eq, Show)

data OpUnary  = UnaryPlus | UnaryNegation | Not
        deriving (Eq, Show)
