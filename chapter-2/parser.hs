-- Chapter 2 code block

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space

main :: IO ()
main = do 
         (expr:_) <- getArgs
         putStrLn (readExpr expr)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many $ escapedChar <|> noneOf "\"\\"
                char '"'
                return $ String x

escapedChar :: Parser Char
escapedChar = do
                char '\\'
                escapedChar <- oneOf "\"\\nrt"
                return $ case escapedChar of 
                    '\\' -> escapedChar
                    '\"' -> escapedChar
                    'n' -> '\n'
                    'r' -> '\r'
                    't' -> '\t'

parseAtom :: Parser LispVal
parseAtom = do 
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of 
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do
                digitSequence <- many1 digit
                let number = read digitSequence
                return $ Number number

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber


