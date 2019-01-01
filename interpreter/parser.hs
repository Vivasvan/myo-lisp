-- Chapter 2 code block

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

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
              return $ Atom atom

parseBool :: Parser LispVal
parseBool = do
                char '#'
                (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False)) 

parseNumber :: Parser LispVal
parseNumber = do
                digitSequence <- many1 digit
                let number = read digitSequence
                return $ Number number

-- parseDecNumber :: Parser LispVal
-- parseDecNumber = do 
--                 decNumberLiteral <- string "#d"
--                 digits <- many1 digit
--                 numberValue <- read digits
--                 return $ Number numberValue

-- parseHexNumber :: Parser LispVal
-- parseHexNumber = do 
--                 hexNumberLiteral <- string "#x"
--                 hexDigits <- many1 hexDigit                
--                 numberValue <- readHex hexDigits
--                 return $ Number numberValue

parseOctNumber :: Parser LispVal
parseOctNumber = do try $ string "#o"
                    digitSequence <- many1 octDigit
                    let literal = digitSequence
                    return $ Number (dig2oct literal)

dig2oct = \x -> fst $ readOct x !! 0  

parseExpr :: Parser LispVal
parseExpr = parseBool  
         <|> parseAtom
         <|> parseString
         <|> parseNumber   



