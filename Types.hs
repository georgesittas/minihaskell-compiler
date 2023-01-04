module Types where

{- Functional Types -}

-- AST representation of the source language (a small subset of Haskell).
type FProgram = (FExpr, [FDefinition])

-- Example: the function definition "foo(x, y) = x + y" is parsed into
-- ("foo", ["x", "y"], (BinaryOp Plus (ExprIdentifier "x") (ExprIdentifier "y")).
type FDefinition = (String, [String], FExpr)

data FExpr
        = FVar String
        | FNum Int
        | FBool Bool
        | FParens FExpr
        | FIfThenElse FExpr FExpr FExpr
        | FCall String [FExpr]
        | FCompOp OpCompare FExpr FExpr
        | FBinaryOp OpBinary FExpr FExpr
        | FBooleanOp OpBool FExpr FExpr
        | FUnaryOp OpUnary FExpr
        deriving (Eq, Show)

{- Intensional Types -}

type IProgram = [IDefinition]

type IDefinition = (String, IExpr)

type IEnv = [Int]

data IExpr
        = IVar String
        | INum Int
        | IBool Bool
        | IParens IExpr
        | IIfThenElse IExpr IExpr IExpr
        | ICall Int String
        | IActuals [IExpr]
        | ICompOp OpCompare IExpr IExpr
        | IBinaryOp OpBinary IExpr IExpr
        | IBooleanOp OpBool IExpr IExpr
        | IUnaryOp OpUnary IExpr
        deriving (Eq, Show)

{- Common Types -}

data OpBinary = Plus | Mult | Minus | Div
        deriving (Eq, Show)

data OpCompare = LtEq | Lt | GtEq | Gt | Eq | Neq
        deriving (Eq, Show)

data OpBool = And | Or
        deriving (Eq, Show)

data OpUnary = Positive | Negative | Not
        deriving (Eq, Show)
