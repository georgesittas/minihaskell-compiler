import Types
import Parser (parsePrettyFile, programParser)
import Transform (transform)
import Intensional (eval)
import Debug.Trace

main :: IO()
main = do
        contents <- getContents
        case programParser contents of
            Left err -> error ("Parse Error")
            Right fp -> print ( transform (trace ("print the fp " ++ show fp) fp) )
