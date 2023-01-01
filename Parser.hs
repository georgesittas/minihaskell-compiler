-----------------------------------------------------------------------------------------------------------------------
--- This module implements a parser for a subset of the haskell language represented by the Program type (Types.hs) ---
-----------------------------------------------------------------------------------------------------------------------

module Parser (
	parseExpression,
	parseProgram,
	programParser,
	exprParser,
	parseFile,
	exprPrettyParser,
	parsePrettyFile,
	programPrettyParser
) where

import Types 
import Control.Monad
import Text.Parsec 
import qualified Text.Parsec.Expr as E
import Data.Char
import Text.Parsec.String (Parser)
import Control.Monad.Identity
import qualified Text.Parsec.Prim as Prim
import Text.Pretty.Simple ( pPrint )


-----------------------------------------------------------------------------------------
--------------------- Assisting functions used for tokens and terms ---------------------
-----------------------------------------------------------------------------------------

-- Consumes all whitespace characters in the input stream.
whitespace :: Parser ()
whitespace = void $ many $ oneOf " \t\r\n"

-- Invokes a given Parser and then consumes any following whitespace.
-- The returned value is whatever the parser returns.
lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

-- Consumes a single symbol lexeme.
symbol :: Char -> Parser ()
symbol ch = void $ lexeme $ char ch

-- Consumes a given keyword k if matched, or fails without consuming any input.
keyword :: String -> Parser String
keyword k = try $ k <$ do { string k ; notFollowedBy $ digit <|> letter <|> char '_'}

-- Similar to keyword, but instead only parses operator characters instead of an identifier.
operator :: String -> Parser String
operator s = try $ lexeme $ s <$ do {string s ; notFollowedBy (oneOf "+-*^/<>=|&")}

-- Parses an identifier: [a-zA-Z_][a-zA-Z0-9_]
identifier :: Parser Identifier
identifier = lexeme $ (:) <$> firstLetter <*> many restLetters
    where   firstLetter = letter <|> char '_'
            restLetters = digit  <|> firstLetter

-- Creates an identifier parser that consumes its input only if it doesn't correspond to a keyword.
blackListIdentifier :: [String] -> Parser String
blackListIdentifier blackList = try $ do {s <- identifier ; guard (s `notElem` blackList) ; return s}


------------------------------------------------------------------------------------------
----------------------- Construct the term parser of an expression -----------------------
------------------------------------------------------------------------------------------

-- A list of keywords which will not be considered as identifiers
keywords :: [String]
keywords = ["False", "True", "else", "if", "not", "result", "then"]

-- Parses identifiers, but not keywords.
identifierToken :: Parser String
identifierToken = blackListIdentifier keywords

-- Parses number literals.
numberToken :: Parser Expression
numberToken = lexeme $ ExprNum . read <$> many1 digit

-- Parses the keywords "True" and "False".
boolToken :: Parser Expression
boolToken = lexeme $ ExprBool . read <$> (keyword "True" <|> keyword "False") 

-- Parses an expression wrapped in parentheses recursively.
parens :: Parser Expression 
parens = try $ ExprParens <$> (symbol '(' *> parseExpression <* symbol ')')

-- Parses an if-then-else expression.
ifThenElse :: Parser Expression
ifThenElse = do void $ lexeme $ keyword "if"
                expr1 <- parseExpression
                void $ lexeme $ keyword "then"
                expr2 <- parseExpression
                void $ lexeme $ keyword "else"
                IfThenElse expr1 expr2 <$> parseExpression

-- Parses a call expression given the function's identifier.
callExpr :: String -> Parser Expression
callExpr s = do symbol '('
                xs <- parseExpression `sepBy` symbol ','
                Call s xs  <$ symbol ')'

-- This parser invokes the call parser and if it fails, then it invokes the identifierToken parser.
-- However, both of these parsers work by first invoking the identifierToken parser. Thus, to avoid
-- unnecessary backtracking, term0 factors the identifierToken parser invocation (left-factoring).
term0 :: Parser Expression
term0 = do s <- identifierToken
           callExpr s <|> return (ExprIdentifier s)

-- Parses an expression term.
term :: Parser Expression
term = choice [ifThenElse, numberToken, boolToken, parens, term0]


------------------------------------------------------------------------------------
------------------  Adding the operators to the expression parser ------------------
------------------------------------------------------------------------------------

-- The following implements a table-driven parsing for operators.
operatorTable :: E.OperatorTable String () Identity Expression
operatorTable = [
	[
		prefix  "+"   $ UnaryOp UnaryPlus,
		prefix  "-"   $ UnaryOp UnaryNegation
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
	],
	[
		prefixK "not" $ UnaryOp Not
	],
	[
		binary "||" (BooleanOP Or ) E.AssocLeft,
		binary "&&" (BooleanOP And) E.AssocLeft
    ]
] where prefix  s f = E.Prefix (f <$ operator s)
		prefixK k f = E.Prefix (f <$ keyword  k)
		binary  s f = E.Infix  (f <$ operator s)
		binaryK k f = E.Infix  (f <$ keyword  k)

-- Builds an expression parser given the operator precedence table and the term parser.
parseExpression :: Parser Expression
parseExpression = E.buildExpressionParser operatorTable term


-----------------------------------------------------------------------------------------
------------------------- The construction of the programParser -------------------------
-----------------------------------------------------------------------------------------

defParser :: Parser FunctionDef
defParser = lexeme $ do s <- identifierToken <* symbol '('
                        xs <- identifierToken `sepBy` symbol ','
                        void $ symbol ')' <* symbol '='
                        expr <- parseExpression
                        FunctionInfo s xs expr <$ (eof <|> symbol ';')

defResultParser :: Parser ResultExpression
defResultParser = lexeme $ do   void $ whitespace *> lexeme (keyword "result") <* symbol '='
                                parseExpression <* (eof <|> symbol ';')

parseProgram :: Parser Program
parseProgram = Program <$> defResultParser <*> many defParser


--------------------------------------------------------------------------------------------
------------------------------ Functions that run the parsers ------------------------------
--------------------------------------------------------------------------------------------

-- Given a Parser p, modify it so that before using p all whitespace is consumed. After the parsing
-- finishes, check if the input has been completely consumed; if not, the parser will fail.
mkParser :: Parser a -> String -> Either ParseError a
mkParser p = parse (whitespace *> p <* eof) ""

-- Given a string, run the expression parser.
exprParser :: String -> Either ParseError Expression
exprParser = mkParser parseExpression

-- Given a string, run the program parser.
programParser :: String -> Either ParseError Program
programParser = mkParser parseProgram

-- Given a file path, run the program parser on that file.
parseFile :: FilePath -> IO (Either ParseError Program)
parseFile = fmap programParser . readFile 


--------------------------------------------------------------------------------------------------
-- A variety of the 3 parsers using the pPrint for a more readable representation of the output --
--------------------------------------------------------------------------------------------------

mkPrettyParser :: (Show a) => Parser a -> String -> IO ()
mkPrettyParser p = pPrint . mkParser p

exprPrettyParser :: String -> IO ()
exprPrettyParser = mkPrettyParser parseExpression

programPrettyParser :: String -> IO ()
programPrettyParser = mkPrettyParser parseProgram

parsePrettyFile :: FilePath -> IO ()
parsePrettyFile path = readFile path >>= programPrettyParser
