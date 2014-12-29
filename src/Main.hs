import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- Parsec already has 'lexeme' parser but we build our own spaces function
-- for pedagogical purposes
spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right _ -> "Found value"

main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr $ head args)
