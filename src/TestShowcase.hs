{- Usage: ./a.out < input.txt -}

import Types
import Parser (programParser)
import Transform (transform)
import Intensional (eval)
import Text.Pretty.Simple (pPrint)

main :: IO()
main = do
        contents <- getContents
        case programParser contents of
            Left err -> error ("> Parse Error")
            Right fp ->
                do
                    putStrLn("> Pretty-Print the MiniHaskell AST:\n")
                    pPrint fp
                    putStrLn("\n\n> Pretty-Print the Intensional AST:\n")
                    pPrint (transform fp)
                    putStrLn("\n\n> Result is:")
                    case eval (transform fp) of
                        INum n -> print n
                        _ -> error ("> Runtime Error")
