module Types where

{- Recommended type signatures for functions "transform" and "eval":

    -- Transforms a MiniHaskell program into an intensional one.
    transform :: FProgram -> IProgram

    -- Evaluates an intensional program and returns the resulting expression.
    eval :: IProgram -> IExpr
-}

{- Functional Types -}

-- AST representation of the source language (a small subset of Haskell).
-- FExpr contains the AST representation of the expression assigned in the "result" function.
-- [FDefinition] contains the AST representation of every other function definition.
type FProgram = (FExpr, [FDefinition])

-- FDefinition consists of the following triplet: 
--      (Function Name, [Typical Parameters], AST Representation of Assigned Expression)
-- Example: the function definition "foo(x, y) = x + y" is parsed into
--      ("foo", ["x", "y"], (FBinaryOp Plus (FVar "x") (FVar "y")).
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

-- AST representation of the intermediate (Intensional) language.
type IProgram = [IDefinition]

-- IDefinition consists of the pair:
--      (Function Name, AST Representation of Intensional Expression)
-- Example: the function definition "foo = x + y" is parsed into
--      ("foo", (IBinaryOp Plus (IVar "x") (IVar "y")).
type IDefinition = (String, IExpr)

-- IEnv represents the "tags" environment variable to be used by the Intensional evaluator.
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
