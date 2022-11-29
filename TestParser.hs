import Parser (parsePrettyFile)
import System.Environment
import System.IO (isEOF)
import Control.Monad ( void )

main :: IO ()
main = do
        iseof <-isEOF
        if iseof
        then    void $ return "End of execution" 
        else    do  line <- getLine
                    if line == "exit"
                    then void $ putStrLn "End of execution"
                    else parsePrettyFile line >> putStrLn "\n" >> main 