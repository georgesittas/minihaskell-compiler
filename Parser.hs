module Parser where

import Control.Applicative
import Control.Monad
import Text.Parsec 
import qualified Text.Parsec.Expr as E
import Data.Char
import Text.Parsec.String (Parser)
import Control.Monad.Identity
import qualified Text.Parsec.Prim as Prim
import Text.Pretty.Simple

type Identifier = String --a identifier (a non-empty string made of integers, letters and underscores but its first char is not a digit)
type NumberLiteral = String -- The string representation of a numerical value (saved as a string for polymorphism)
type Var = String
type ResultExpression = Expression

data Program =  Program ResultExpression [FunctionDef] 
        deriving (Eq, Show)

data FunctionDef =  FunctionInfo Identifier [Var] Expression 
        deriving (Eq, Show)

data Expression =   ExprIdentifier Identifier
                |   ExprNum NumberLiteral
                |   ExprString String  -- the String literal in other words a string  whose is enclosed left and right by double quotes (ex. "a stringliteral!!")
                |   ExprBool Bool
                |   ExprParens Expression
                |   CompOp   OpCompare Expression Expression -- Comparison operators, <=, >=, >, <, ==
                |   BinaryOp OpBinary Expression Expression  -- Arithmetic Binary Operators + - * ^ /
                |   BooleanOP OpBool Expression Expression   -- Boolean binary operators && and || 
                |   UnaryOp  OpUnary  Expression             -- Unary operators
                |   IfThenElse Expression Expression Expression
                |   Parens Expression
                |   Call Identifier [Expression]
        deriving (Eq, Show)

data OpBinary = Plus | Mult | Minus | Exp | Div -- representing (+), (*), (-), (^), (/)
        deriving (Eq, Show)

data OpCompare = OpLTEQ | OpLT | OpGTEQ | OpGT | OpEQ
        deriving (Eq, Show)

data OpBool = And | Or
        deriving (Eq, Show)

data OpUnary  = UnaryPlus | UnaryNegation | Not  -- representing +, - , `not` .  The first 2 are numerical operators, the last one is a binary one
        deriving (Eq, Show)

-- Tokens

--Given a list of forbidden words (keywords), the parser detects when an identifier is actually a keyword or not
-- If it a keyword, it fails without consuming input otherwise create the identifier
blackListIdentifier :: [String] -> Parser String
blackListIdentifier blackList = try $ do {s <- identifier ; guard (s `notElem` blackList) ; return s}

identifierToken :: Parser String
identifierToken = blackListIdentifier keywords

--TODO: Do I add the function name "result"?
keywords :: [String]
keywords = ["not", "if", "then", "else", "True", "False"];

whitespace :: Parser ()
whitespace = void $ Prim.many $ oneOf " \t\r\n"


lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

identifier :: Parser Identifier
identifier = lexeme $ (:) <$> firstLetter <*> Prim.many restLetters
    where   firstLetter = letter Prim.<|> char '_'
            restLetters = digit  Prim.<|> firstLetter

numberToken :: Parser Expression
numberToken = lexeme $ ExprNum <$> many1 digit

symbol :: Char -> Parser ()
symbol s = void $ lexeme $ char s

-- TODO: Handle properly escape characters in string?
stringToken :: Parser Expression
stringToken = lexeme $ ExprString <$> (char '\"' *> manyTill (Prim.many (char '\\') *> anyChar) (char '\"'))

boolToken :: Parser Expression
boolToken = lexeme $ ExprBool . read <$> (keyword "True" Prim.<|> keyword "False") 

keyword :: String -> Parser String
keyword k = try $ k <$ do { string k ; notFollowedBy $ alphaNum Prim.<|> char '_'}

operator :: String -> Parser String
operator s = try $ lexeme $ s <$ do {string s ; notFollowedBy ( oneOf "+-*^/<>=|&")}

parens :: Parser Expression 
parens = Parens <$> (symbol '(' *> parseExpression <* symbol ')')

ifThenElse :: Parser Expression
ifThenElse = do void $ lexeme $ keyword "if"
                expr1 <- parseExpression
                void $ lexeme $ keyword "then"
                expr2 <- parseExpression
                void $ lexeme $ keyword "else"
                IfThenElse expr1 expr2 <$> parseExpression

callExpr :: Parser Expression
callExpr = do   s <- identifierToken <* symbol '('
                xs <- parseExpression `sepBy` symbol ','
                Call s xs  <$ symbol ')'

defParser :: Parser FunctionDef
defParser = lexeme $ do s <- identifierToken <* symbol '('
                        xs <- identifierToken `sepBy` symbol ','
                        void $ symbol ')' <* symbol '='
                        expr <- parseExpression
                        FunctionInfo s xs expr <$ (eof Prim.<|> symbol ';')

defResultParser :: Parser ResultExpression
defResultParser = lexeme $ do   void $ whitespace *> lexeme (keyword "result") <* symbol '='
                                parseExpression <* (eof Prim.<|> symbol ';')


term :: Parser Expression
term = choice [ifThenElse, numberToken, boolToken, ExprIdentifier <$> identifierToken, stringToken, parens, callExpr]


operatorTable :: E.OperatorTable String () Identity Expression
operatorTable = [   [
                    prefixK "not" $ UnaryOp Not, 
                    prefix  "+"   $ UnaryOp UnaryPlus, 
                    prefix  "-"   $ UnaryOp UnaryNegation
                    ],
                    [
                    binary "||" (BooleanOP Or ) E.AssocLeft,
                    binary "&&" (BooleanOP And) E.AssocLeft
                    ],
                    [
                    binary "^" (BinaryOp Exp) E.AssocRight
                    ],
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
                    ]
                ]
        where   prefix  s f = E.Prefix (f <$ operator s)
                prefixK k f = E.Prefix (f <$ keyword  k)
                binary  s f = E.Infix  (f <$ operator s)
                binaryK k f = E.Infix  (f <$ keyword  k)




parseExpression :: Parser Expression
parseExpression = E.buildExpressionParser operatorTable term

mkParser :: (Show a) => Parser a -> String -> IO ()
mkParser p = pPrint . parse (whitespace *> p) ""

exprParser :: String -> IO ()
exprParser = mkParser parseExpression

parseProgram :: Parser Program
parseProgram = Program <$> defResultParser <*> Prim.many defParser

programParser :: String -> IO ()
programParser = mkParser parseProgram
