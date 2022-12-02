module ParserUnitTests 
(runTests, runProgramTests)
where

import Parser
import Types
import Text.Parsec.String
import Text.Parsec
import qualified Test.HUnit as H


makeTest :: (Eq a, Show a) => Parser a -> (String,a) -> H.Test
makeTest parser (src,expected) = H.TestLabel src $ H.TestCase $ do
    let gote = parse parser "" src
    case gote of
      Left e -> H.assertFailure $ show e
      Right got -> H.assertEqual src expected got


runTests :: (Eq a, Show a) => Parser a -> [(String, a)] -> IO H.Counts
runTests p = H.runTestTT . H.TestList . map (makeTest p)


runProgramTests :: [(String, Program)] -> IO H.Counts
runProgramTests = runTests parseProgram