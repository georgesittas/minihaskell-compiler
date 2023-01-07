-----------------------------------------------------------------------------------------------------------------------
--- This module implements a parser for a subset of the haskell language represented by the FProgram type (Types.hs) ---
-----------------------------------------------------------------------------------------------------------------------

module Parser (
    parseExpression,
    parseProgram,
    programParser,
    parseFile,
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
identifier :: Parser String
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
numberToken :: Parser FExpr
numberToken = lexeme $ FNum . read <$> many1 digit

-- Parses the keywords "True" and "False".
boolToken :: Parser FExpr
boolToken = lexeme $ FBool . read <$> (keyword "True" <|> keyword "False") 

-- Parses an expression wrapped in parentheses recursively.
parens :: Parser FExpr 
parens = try $ FParens <$> (symbol '(' *> parseExpression <* symbol ')')

-- Parses an if-then-else expression.
ifThenElse :: Parser FExpr
ifThenElse = do void $ lexeme $ keyword "if"
                expr1 <- parseExpression
                void $ lexeme $ keyword "then"
                expr2 <- parseExpression
                void $ lexeme $ keyword "else"
                FIfThenElse expr1 expr2 <$> parseExpression

-- Parses a call expression given the function's identifier.
callExpr :: String -> Parser FExpr
callExpr s = do symbol '('
                xs <- parseExpression `sepBy` symbol ','
                FCall s xs  <$ symbol ')'

-- This parser invokes the call parser and if it fails, then it invokes the identifierToken parser.
-- However, both of these parsers work by first invoking the identifierToken parser. Thus, to avoid
-- unnecessary backtracking, term0 factors the identifierToken parser invocation (left-factoring).
term0 :: Parser FExpr
term0 = do s <- identifierToken
           callExpr s <|> return (FVar s)

-- Parses an expression term.
term :: Parser FExpr
term = choice [ifThenElse, numberToken, boolToken, parens, term0]


------------------------------------------------------------------------------------
------------------  Adding the operators to the expression parser ------------------
------------------------------------------------------------------------------------

-- The following implements a table-driven parsing for operators.
operatorTable :: E.OperatorTable String () Identity FExpr
operatorTable =
    [
        [
            prefix  "+"   $ FUnaryOp Positive,
            prefix  "-"   $ FUnaryOp Negative
        ],
        [
            binary "*" (FBinaryOp Mult) E.AssocLeft,
            binary "/" (FBinaryOp Div) E.AssocLeft
        ],
        [
            binary "+" (FBinaryOp Plus) E.AssocLeft,
            binary "-" (FBinaryOp Minus) E.AssocLeft
        ],
        [
            binary "<="  (FCompOp LtEq) E.AssocNone,
            binary ">=" (FCompOp GtEq) E.AssocNone,
            binary "<" (FCompOp Lt) E.AssocNone,
            binary ">" (FCompOp Gt) E.AssocNone,
            binary "==" (FCompOp Eq) E.AssocNone,
            binary "/=" (FCompOp Neq) E.AssocNone
        ],
        [
            prefixK "not" $ FUnaryOp Not
        ],
        [
            binary "||" (FBooleanOp Or ) E.AssocLeft,
            binary "&&" (FBooleanOp And) E.AssocLeft
        ]
    ]
    where
        prefix  s f = E.Prefix (f <$ operator s)
        prefixK k f = E.Prefix (f <$ keyword  k)
        binary  s f = E.Infix  (f <$ operator s)
        binaryK k f = E.Infix  (f <$ keyword  k)

-- Builds an expression parser given the operator precedence table and the term parser.
parseExpression :: Parser FExpr
parseExpression = E.buildExpressionParser operatorTable term


-----------------------------------------------------------------------------------------
------------------------- The construction of the programParser -------------------------
-----------------------------------------------------------------------------------------

defParser :: Parser FDefinition
defParser = lexeme $ do s <- identifierToken <* symbol '('
                        xs <- identifierToken `sepBy` symbol ','
                        void $ symbol ')' <* symbol '='
                        expr <- parseExpression
                        (s, xs, expr) <$ (eof <|> symbol ';')

defResultParser :: Parser FExpr
defResultParser = lexeme $ do   void $ whitespace *> lexeme (keyword "result") <* symbol '='
                                parseExpression <* (eof <|> symbol ';')

parseProgram :: Parser FProgram
parseProgram = (,) <$> defResultParser <*> many defParser


--------------------------------------------------------------------------------------------
------------------------------ Functions that run the parsers ------------------------------
--------------------------------------------------------------------------------------------

-- Given a Parser p, modify it so that before using p all whitespace is consumed. After the parsing
-- finishes, check if the input has been completely consumed; if not, the parser will fail.
mkParser :: Parser a -> String -> Either ParseError a
mkParser p = parse (whitespace *> p <* eof) ""

-- Given a string, run the program parser.
programParser :: String -> Either ParseError FProgram
programParser = mkParser parseProgram

-- Given a file path, run the program parser on that file.
parseFile :: FilePath -> IO (Either ParseError FProgram)
parseFile = fmap programParser . readFile 


--------------------------------------------------------------------------------------------------
-- A variety of the 3 parsers using the pPrint for a more readable representation of the output --
--------------------------------------------------------------------------------------------------

mkPrettyParser :: (Show a) => Parser a -> String -> IO ()
mkPrettyParser p = pPrint . mkParser p

programPrettyParser :: String -> IO ()
programPrettyParser = mkPrettyParser parseProgram

parsePrettyFile :: FilePath -> IO ()
parsePrettyFile path = readFile path >>= programPrettyParser
