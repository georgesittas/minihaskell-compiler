module Parser 
(
programParser,
exprParser,
parseFile,
exprPrettyParser,
parsePrettyFile,
programPrettyParser
)

where

import Control.Monad
import Text.Parsec 
import qualified Text.Parsec.Expr as E
import Data.Char
import Text.Parsec.String (Parser)
import Control.Monad.Identity
import qualified Text.Parsec.Prim as Prim
import Text.Pretty.Simple ( pPrint )

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

data OpBinary = Plus | Mult | Minus | Div {- | Exp -} -- representing (+), (*), (-), (^), (/) respectively
        deriving (Eq, Show)

data OpCompare = OpLTEQ | OpLT | OpGTEQ | OpGT | OpEQ | OpNEQ -- representing (<=), (<), (>=), (>), (==), (/=)
        deriving (Eq, Show)

data OpBool = And | Or                          -- no need for a comment :P
        deriving (Eq, Show)

data OpUnary  = UnaryPlus | UnaryNegation | Not  -- representing the unary +, - , `not` respectively.  The first 2 are numerical operators, the last one is a binary one
        deriving (Eq, Show)

-----------------------------------------------------------------------------------------------
--------------------- Assisting functions used for tokens and terms ---------------------------
-----------------------------------------------------------------------------------------------

-- Given a list of forbidden words (keywords), the parser detects when an identifier is actually a keyword or not
-- If it a keyword, it fails without consuming input otherwise parse successfully and create the identifier
blackListIdentifier :: [String] -> Parser String
blackListIdentifier blackList = try $ do {s <- identifier ; guard (s `notElem` blackList) ; return s}

identifierToken :: Parser String
identifierToken = blackListIdentifier keywords

--TODO: Do I add the function name "result"?
-- A list of keywords which will not be considered as identifiers
keywords :: [String]
keywords = ["not", "if", "then", "else", "True", "False"];

-- A Parser whose job is to consume as many whitespace characters as it can, till it reaches a non-whitespace character.
-- It will succeed even if there is no whitespace to consume, however, since whatever the parser returns as output is irrelevant, the type () is used for a dummy value
whitespace :: Parser ()
whitespace = void $ many $ oneOf " \t\r\n"

-- lexeme runs the parser p, and after it finishes, if it succeeds, it runs the whitespace parser but keeps the output of the parser p and ignore the output of the whitespace parser
-- In other words, lexeme is used for making a parser p to consume all whitespace after it ends.
lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

-- A parser whose job is to parse and return an identifier. 
identifier :: Parser Identifier
identifier = lexeme $ (:) <$> firstLetter <*> many restLetters
    where   firstLetter = letter <|> char '_'
            restLetters = digit  <|> firstLetter

-- symbol ch parses the character ch, then consumes all whitespace and then ignores the otput by returning the dummy value ()
symbol :: Char -> Parser ()
symbol ch = void $ lexeme $ char ch

-- A special parser whose job is to parse the string k. However, it is possible that the next character of the input is also an letter,digit or '_' so check if that is the case
--  e.g "else_1" is considered as an identifier even though its first 4 characters is the keyword "else"
-- If it is not the case, it succeeds and returns the keyword. Otherwise, fail without consuming input
keyword :: String -> Parser String
keyword k = try $ k <$ do { string k ; notFollowedBy $ digit <|> letter <|> char '_'}

-- Similar to keyword, but instead only parses operator characters instead of an identifier.
operator :: String -> Parser String
operator s = try $ lexeme $ s <$ do {string s ; notFollowedBy ( oneOf "+-*^/<>=|&")}
-- A parser whose job is to parse a number literal and cast it into an Int

-------------------------------------------------------------------------------------------------
----------------------- Construct the term parser of an expression ------------------------------
-------------------------------------------------------------------------------------------------

numberToken :: Parser Expression
numberToken = lexeme $ ExprNum . read <$> many1 digit

{- 
-- TODO: Handle properly escape characters in string?
-- A parser whose job is to parse string tokens. However, it does not parse correctly escape characters
stringToken :: Parser Expression
stringToken = lexeme $ ExprString <$> (char '\"' *> manyTill anyChar (char '\"'))
-}

-- A parser who parses a boolean token
boolToken :: Parser Expression
boolToken = lexeme $ ExprBool . read <$> (keyword "True" <|> keyword "False") 

-- Check for a left parentheses. If yes, then its an expression inside parentheses so recursively run the expression parser and after it finishes check for a right parentheses.
parens :: Parser Expression 
parens = try $ ExprParens <$> (symbol '(' *> parseExpression <* symbol ')')

-- check for the keyword "if". If yes, it it an if-then-else expression
ifThenElse :: Parser Expression
ifThenElse = do void $ lexeme $ keyword "if"
                expr1 <- parseExpression
                void $ lexeme $ keyword "then"
                expr2 <- parseExpression
                void $ lexeme $ keyword "else"
                IfThenElse expr1 expr2 <$> parseExpression

-- A parser for a call expression
callExpr :: Parser Expression
callExpr = do   s <- try $ identifierToken <* symbol '('
                xs <- parseExpression `sepBy` symbol ','
                Call s xs  <$ symbol ')'

--the terms of an expression  made of terms combined with boolean, arithmetic and comparison operators
term :: Parser Expression
term = choice [ifThenElse, numberToken, boolToken, parens, callExpr, ExprIdentifier <$> identifierToken {- ,stringToken -}]

-------------------------------------------------------------------------------------------------
------------------  Adding the operators to the expression parser -------------------------------
-------------------------------------------------------------------------------------------------

-- module Text.parsec.Expr imported as E is a module which supports the construction of a parser, by giving a parser for terms,
-- and an operator_table which is a list which contains lists of all unary and binary operators which will be used.
-- Also it is possible to specify the precedence of the operators, the associativity of binary operators, and whether
-- a unary operator is a prefix or suffix operator.

operatorTable :: E.OperatorTable String () Identity Expression
operatorTable = [   [ 
                    prefix  "+"   $ UnaryOp UnaryPlus, 
                    prefix  "-"   $ UnaryOp UnaryNegation
                    ],
                    --[
                    --binary "^" (BinaryOp Exp) E.AssocRight
                    --],
                    [
                    binary "*" (BinaryOp Mult) E.AssocLeft,
                    binary "/" (BinaryOp Div) E.AssocLeft
                    ],
                    [
                    binary "+" (BinaryOp Plus) E.AssocLeft,
                    binary "-" (BinaryOp Minus) E.AssocLeft
                    ],
                    [
                    binary "<="  (CompOp OpLTEQ) E.AssocNone, 
                    binary ">=" (CompOp OpGTEQ) E.AssocNone, 
                    binary "<" (CompOp OpLT) E.AssocNone, 
                    binary ">" (CompOp OpGT) E.AssocNone, 
                    binary "==" (CompOp OpEQ) E.AssocNone
                    ],
                    [
                    prefixK "not" $ UnaryOp Not
                    ],
                    [
                    binary "||" (BooleanOP Or ) E.AssocLeft,
                    binary "&&" (BooleanOP And) E.AssocLeft
                    ]
                ]
        where   prefix  s f = E.Prefix (f <$ operator s)
                prefixK k f = E.Prefix (f <$ keyword  k)
                binary  s f = E.Infix  (f <$ operator s)
                binaryK k f = E.Infix  (f <$ keyword  k)



-- Given the parser for terms and an operator table with the necessary information about operators, construct the parser for expressions
parseExpression :: Parser Expression
parseExpression = E.buildExpressionParser operatorTable term


-------------------------------------------------------------------------------------------------
------------------------- The construction of the programParser ---------------------------------
-------------------------------------------------------------------------------------------------

-- A parser for a function definition
defParser :: Parser FunctionDef
defParser = lexeme $ do s <- identifierToken <* symbol '('
                        xs <- identifierToken `sepBy` symbol ','
                        void $ symbol ')' <* symbol '='
                        expr <- parseExpression
                        FunctionInfo s xs expr <$ (eof <|> symbol ';')

--A parser for the result function definition
defResultParser :: Parser ResultExpression
defResultParser = lexeme $ do   void $ whitespace *> lexeme (keyword "result") <* symbol '='
                                parseExpression <* (eof <|> symbol ';')

-- Construct the parser for a Program
parseProgram :: Parser Program
parseProgram = Program <$> defResultParser <*> many defParser


-------------------------------------------------------------------------------------------------
------------------------------ Functions that run the parsers -----------------------------------
-------------------------------------------------------------------------------------------------


--Given a Parser p, modify it so that before using the parser p, whitespace is consumed, and after the parser P finishes, check if all the input has been parsed or not.
-- If not all is parsed, the parser will fail.
-- After modifying the behaviour of the parser p, mkParser p takes a string and executes the modified parser p. It returns the result of the parser
-- In cse of failure at parsing, the type Either ParseError a is returned.

mkParser :: Parser a -> String -> Either ParseError a
mkParser p = parse (whitespace *> p <* eof) ""

-- Given a string, run the expression parser
exprParser :: String -> Either ParseError Expression
exprParser = mkParser parseExpression

-- Given a string, run the program parser
programParser :: String -> Either ParseError Program
programParser = mkParser parseProgram

-- Given a file path, run the program parser on that file
parseFile :: FilePath -> IO (Either ParseError Program)
parseFile = fmap programParser . readFile 


-------------------------------------------------------------------------------------------------
-- A variety of the 3 parsers using the pPrint for a more readable represantiation of the output-
-------------------------------------------------------------------------------------------------


-- same as mkParser but after the parser finishes execution and returns Either ParseError a
-- the pPring function from the module Text.Pretty.Simple is used to pring in the terminal in a more consice way the returned value.
mkPrettyParser :: (Show a) => Parser a -> String -> IO ()
mkPrettyParser p = pPrint . mkParser p


exprPrettyParser :: String -> IO ()
exprPrettyParser = mkPrettyParser parseExpression

programPrettyParser :: String -> IO ()
programPrettyParser = mkPrettyParser parseProgram

parsePrettyFile :: FilePath -> IO ()
parsePrettyFile path = readFile path >>= programPrettyParser