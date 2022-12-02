module Types where

type Identifier = String              -- a identifier (a non-empty string made of integers, letters and underscores but its first char is not a digit)
type NumberLiteral = Int              -- The type assigned to a number literal has been chosen to be an Int 
type Var = String                     -- Var is a nullary variable appearing in the arguments of the definition of a function 
type ResultExpression = Expression    -- This expression is special in the sense that it is the definition of the result function

-- The data type the parser outputs
-- First it contain the expression of the result function
-- Then, after parsing all other function definitions, save them in a list
data Program =  Program ResultExpression [FunctionDef] 
        deriving (Eq, Show)

-- The data type of a function definition
-- Identifier is the name of the  function\
-- [Var] is the list of the nullary variables as they appear in the argument list of a function
-- Expression is the definion of the function
--	e.g for the function `foo(x,y,z) = x + y * z` the FunctionDef after the parsing will be this: 
--		FunctionInfo "foo" ["x","y","z"] (BinaryOp Plus (ExprIdentifier "x") (BinaryOp Mult (ExprIdentifier "y") (ExprIdentifier "z")))
data FunctionDef =  FunctionInfo Identifier [Var] Expression 
        deriving (Eq, Show)

-- the expression data type  (commented out string literals)
data Expression =   ExprIdentifier Identifier                   -- The expression can be an identifier
                |   ExprNum NumberLiteral                       -- It can be a number literal
--                |   ExprString String  		   			    -- It can be a string literal, in other words a string which is enclosed left and right by double quotes (ex. "a stringliteral!!")
                |   ExprBool Bool                               -- It can be a Bool literal, aka it can be True or False
                |   ExprParens Expression                       -- It can be an Expression inside Parentheses
                |   IfThenElse Expression Expression Expression -- It can be an if-then-else expression
                |   Call Identifier [Expression]                -- It can be a function call
                |   CompOp   OpCompare Expression Expression    -- It can be a binary Comparison operator between 2 Expressions. Possible operators: <=, >=, >, <, ==, /=
                |   BinaryOp OpBinary Expression Expression     -- It can be a binary Arithmetic operator between 2 Expressions: Possible Operators +, -, *, /
                |   BooleanOP OpBool Expression Expression      -- It can be a binary Boolean    operator between 2 Expressions: Possible operators && (and), || (or) 
                |   UnaryOp  OpUnary  Expression                -- It can be a unary Operator, either an arithmetic plus or minus (+ or -) or it can be the boolean negation operator "not" 

        deriving (Eq, Show)

data OpBinary = Plus | Mult | Minus | Div {- | Exp -} -- representing (+), (*), (-), (/), (^) respectively
        deriving (Eq, Show)

data OpCompare = OpLTEQ | OpLT | OpGTEQ | OpGT | OpEQ | OpNEQ -- representing (<=), (<), (>=), (>), (==), (/=)
        deriving (Eq, Show)

data OpBool = And | Or                          -- no need for a comment :P
        deriving (Eq, Show)

data OpUnary  = UnaryPlus | UnaryNegation | Not  -- representing the unary +, - , `not` respectively.  The first 2 are numerical operators, the last one is a binary one
        deriving (Eq, Show)